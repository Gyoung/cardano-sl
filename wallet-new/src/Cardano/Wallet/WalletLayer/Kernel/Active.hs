module Cardano.Wallet.WalletLayer.Kernel.Active (
    pay
  , estimateFees
  , createUnsignedTx
  , redeemAda
  ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Default (def)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Second)

import           Pos.Chain.Txp (Tx (..), TxAttributes, TxOut (..),
                     TxOutAux (..), toaOut, txOutAddress)
import           Pos.Core (Address, Coin, TxFeePolicy)
import           Pos.Crypto (PassPhrase)

import           Cardano.Wallet.API.V1.Types (unV1)
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation,
                     InputGrouping, UnsignedTx (..), newOptions)
import           Cardano.Wallet.Kernel.DB.AcidState (DB)
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.Read (lookupCardanoAddress)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import qualified Cardano.Wallet.Kernel.Transactions as Kernel
import qualified Cardano.Wallet.Kernel.Types as Kernel
import           Cardano.Wallet.WalletLayer (EstimateFeesError (..),
                     NewPaymentError (..), NewUnsignedTransactionError (..),
                     RedeemAdaError (..))
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Kernel.Conv

-- | Generates a new transaction @and submit it as pending@.
pay :: MonadIO m
    => Kernel.ActiveWallet
    -> PassPhrase
    -> InputGrouping
    -> ExpenseRegulation
    -> V1.Payment
    -> m (Either NewPaymentError (Tx, TxMeta))
pay activeWallet pw grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $
      runExceptT $ do
        (opts, accId, payees) <- withExceptT NewPaymentWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        withExceptT NewPaymentError $ ExceptT $
          Kernel.pay activeWallet pw opts accId payees

-- | Estimates the fees for a payment.
estimateFees :: MonadIO m
             => Kernel.ActiveWallet
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> m (Either EstimateFeesError Coin)
estimateFees activeWallet grouping regulation payment = liftIO $ do
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
      runExceptT $ do
        (opts, accId, payees) <- withExceptT EstimateFeesWalletIdDecodingFailed $
                                   setupPayment policy grouping regulation payment
        withExceptT EstimateFeesError $ ExceptT $
          Kernel.estimateFees activeWallet opts accId payees

-- | Creates a raw transaction.
--
-- NOTE: this function does /not/ perform a payment, it just creates a new
-- transaction which will be signed and submitted to the blockchain later.
-- It returns a transaction and a list of source addresses and corresponding
-- derivation paths.
createUnsignedTx :: MonadIO m
                 => Kernel.ActiveWallet
                 -> InputGrouping
                 -> ExpenseRegulation
                 -> V1.Payment
                 -> m (Either NewUnsignedTransactionError
                             ( Tx
                             , NonEmpty (Address, [V1.AddressLevel])
                             )
                      )
createUnsignedTx activeWallet grouping regulation payment = liftIO $ do
    let spendingPassword = maybe mempty coerce $ V1.pmtSpendingPassword payment
        walletPassive = Kernel.walletPassive activeWallet
    policy <- Node.getFeePolicy (Kernel.walletPassive activeWallet ^. Kernel.walletNode)
    res <- runExceptT $ do
        (opts, accId, payees) <- withExceptT NewTransactionWalletIdDecodingFailed $
            setupPayment policy grouping regulation payment
        (db, unsignedTx, utxo) <- withExceptT NewUnsignedTransactionError $ ExceptT $
            Kernel.newUnsignedTransaction activeWallet opts accId payees
        return (db, unsignedTx, utxo, accId)
    case res of
        Left e -> return (Left e)
        Right (db, unsignedTx, _utxo, srcAccId) -> do
            -- We have to provide source addresses and derivation paths for this transaction.
            -- It will be used by external party to provide a proof that it has a right to
            -- spend this money.
            let srcAddresses    = extractSrcAddressesFrom unsignedTx
                srcHdAddresses  = NE.map (toHdAddress db) srcAddresses
                derivationPaths = buildDerivationPathsFor srcHdAddresses srcAccId
            -- Now we have to generate the change addresses needed,
            -- because 'newUnsignedTransaction' function cannot do it by itself.
            changeAddressesRes <- runExceptT $ withExceptT Kernel.NewTransactionErrorCreateAddressFailed $
                genChangeOuts (utxChange unsignedTx) srcAccId spendingPassword walletPassive
            case changeAddressesRes of
                Left e -> return (Left $ NewUnsignedTransactionError e)
                Right changeAddresses -> do
                    let allOutputs = (NE.toList $ utxOutputs unsignedTx) ++ changeAddresses
                        unsignedTxWithChange = unsignedTx { utxOutputs = NE.fromList allOutputs }
                        tx = toRegularTx unsignedTxWithChange
                    return (Right (tx, NE.zip srcAddresses derivationPaths))
  where
    -- | Generates the list of change outputs from a list of change coins.
    genChangeOuts :: MonadIO m
                  => [Coin]
                  -> HD.HdAccountId
                  -> PassPhrase
                  -> Kernel.PassiveWallet
                  -> ExceptT Kernel.CreateAddressError m [TxOutAux]
    genChangeOuts changeCoins srcAccountId spendingPassword walletPassive =
        forM changeCoins $ \change -> do
            changeAddr <- genChangeAddr srcAccountId spendingPassword walletPassive
            return TxOutAux {
                toaOut = TxOut
                    { txOutAddress = changeAddr
                    , txOutValue   = change
                    }
            }

    genChangeAddr :: MonadIO m
                  => HD.HdAccountId
                  -> PassPhrase
                  -> Kernel.PassiveWallet
                  -> ExceptT Kernel.CreateAddressError m Address
    genChangeAddr srcAccountId spendingPassword walletPassive = ExceptT $ liftIO $
        Kernel.createAddress spendingPassword
                             (Kernel.AccountIdHdRnd srcAccountId)
                             walletPassive

    -- Take addresses that correspond to inputs of transaction.
    extractSrcAddressesFrom :: UnsignedTx -> NonEmpty Address
    extractSrcAddressesFrom unsignedTx =
        NE.map (\(_, outAux) -> txOutAddress . toaOut $ outAux) $ utxOwnedInputs unsignedTx

    buildDerivationPathsFor :: NonEmpty HD.HdAddress
                            -> HD.HdAccountId
                            -> NonEmpty [V1.AddressLevel]
    buildDerivationPathsFor srcAddresses srcAccountId =
        flip NE.map srcAddresses $ \(HD.HdAddress srcAddressId _) ->
            let HD.HdAccountId _ (HD.HdAccountIx srcAccIndex)  = srcAccountId
                HD.HdAddressId _ (HD.HdAddressIx srcAddrIndex) = srcAddressId
            in [ V1.word32ToAddressLevel srcAccIndex
               , V1.word32ToAddressLevel srcAddrIndex
               ]

    toHdAddress :: DB
                -> Address
                -> HD.HdAddress
    toHdAddress db srcAddress =
        -- Sine 'srcAddress' is returned by 'newUnsignedTransaction'
        -- we assume that this address is valid so we definitely have
        -- corresponding 'HdAddress'.
        either (error "Impossible happened: source address doesn't have corresponding HD-address!")
               id
               (lookupCardanoAddress db srcAddress)

    -- Convert 'UnsignedTx' to 'Tx', this regular 'Tx'
    -- will be signed later (technically - the hash of the
    -- transaction will be signed).
    toRegularTx :: UnsignedTx -> Tx
    toRegularTx unsignedTx =
        let inputs  = NE.map fst $ utxOwnedInputs unsignedTx
            outputs = NE.map toaOut $ utxOutputs unsignedTx
            -- Currently all transactions have default (actually - empty) attributes.
            attribs = def :: TxAttributes
        in UnsafeTx inputs outputs attribs

-- | Redeem an Ada voucher
--
-- Implementation note: No need for a time limit here, redemption does not run
-- coin selection.
redeemAda :: MonadIO m
          => Kernel.ActiveWallet
          -> V1.Redemption
          -> m (Either RedeemAdaError (Tx, TxMeta))
redeemAda aw
          V1.Redemption{
              redemptionRedemptionCode   = code
            , redemptionMnemonic         = mMnemonic
            , redemptionSpendingPassword = V1.V1 spendingPassword
            , redemptionWalletId         = wId
            , redemptionAccountIndex     = accIx
            } = runExceptT $ do
    accId <- withExceptT RedeemAdaWalletIdDecodingFailed $
               fromAccountId wId accIx
    case mMnemonic of
      Nothing -> do
        redeemKey <- withExceptT RedeemAdaInvalidRedemptionCode $
                       fromRedemptionCode code
        withExceptT RedeemAdaError $ ExceptT $ liftIO $
          Kernel.redeemAda aw accId spendingPassword redeemKey
      Just mnemonic -> do
        redeemKey <- withExceptT RedeemAdaInvalidRedemptionCode $
                       fromRedemptionCodePaper code mnemonic
        withExceptT RedeemAdaError $ ExceptT $ liftIO $
          Kernel.redeemAda aw accId spendingPassword redeemKey

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: Monad m
             => TxFeePolicy
             -> InputGrouping
             -> ExpenseRegulation
             -> V1.Payment
             -> ExceptT Text m ( CoinSelectionOptions
                               , HD.HdAccountId
                               , NonEmpty (Address, Coin)
                               )
setupPayment policy grouping regulation payment = do
    rootId <- fromRootId wId
    let opts   = (newOptions (Kernel.cardanoFee policy)) {
                     csoExpenseRegulation = regulation
                   , csoInputGrouping     = grouping
                   }
        accIx  = HD.HdAccountIx (V1.getAccIndex . V1.psAccountIndex . V1.pmtSource $ payment)
        accId  = HD.HdAccountId {
                     _hdAccountIdParent = rootId
                   , _hdAccountIdIx     = accIx
                   }
        payees = (\(V1.PaymentDistribution a c) -> (unV1 a, unV1 c)) <$>
                   V1.pmtDestinations payment
    return (opts, accId, payees)
  where
    wId = V1.psWalletId . V1.pmtSource $ payment
