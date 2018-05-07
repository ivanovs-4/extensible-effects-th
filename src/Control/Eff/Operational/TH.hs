module Control.Eff.Operational.TH
    ( mkProgramOps
    )
    where

import Control.Monad
import Data.Char (toLower)
import Data.Monoid
import Language.Haskell.TH

trace :: Show a => a -> Q ()
trace a = reportWarning (show a)

failMaybe :: Monad m => String -> m (Maybe a) -> m a
failMaybe error m = m >>= maybe (fail error) return

operationName :: String -> String
operationName (c:rest) = toLower c : rest

liftCon' :: [TyVarBndr] -> [Type] -> Type -> Name -> Name -> [Type] -> Type -> Q [Dec]
liftCon' kindeds constraints functor nextTyName conName fieldTys tyPa = do
    let liftedName = mkName (operationName (nameBase conName))
    eff <- failMaybe "Eff not in scope" $ lookupTypeName "Eff"
    program <- failMaybe "`Program` not in scope" $ lookupTypeName "Program"
    member <- failMaybe "`Member` not in scope" $ lookupTypeName "Member"
    singleton <- failMaybe "`singleton` not in scope" $ lookupValueName "singleton"
    r <- newName "r"
    paramNames <- sequence $ const (newName "x") <$> fieldTys
    pure
        [ SigD liftedName
            (ForallT ([]
                     )
                     ([AppT (AppT (ConT member) (AppT (ConT program) functor))
                           (VarT r)
                      ] <> constraints
                     )
                     (flip (foldr (\el ts -> AppT (AppT ArrowT el) ts))
                         fieldTys
                         (AppT (AppT (ConT eff) (VarT r))
                               tyPa
                         )
                     )
            )
        , FunD liftedName
               [ Clause ( VarP <$> paramNames )
                        ( NormalB ( AppE ( VarE singleton )
                                         ( foldl AppE (ConE conName) (VarE <$> paramNames) )
                                  )
                        )
                        []
               ]
        ]

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name)    = name
tyVarBndrName (KindedTV name _) = name

liftCon :: Type -> Name -> Con -> Q [Dec]
liftCon functor nextTyName (
      GadtC conNames
            fieldTys
            (AppT tyName tyPa)
      ) = fmap mconcat . forM conNames $ \conName ->
    liftCon' [] [] functor nextTyName conName (snd <$> fieldTys) tyPa
liftCon functor nextTyName (
      ForallC kindeds constraints
      ( GadtC conNames
              fieldTys
              (AppT tyName tyPa)
      ) ) = fmap mconcat . forM conNames $ \conName ->
    liftCon' kindeds constraints functor nextTyName conName (snd <$> fieldTys) tyPa
liftCon _ _ con = fail $ "liftCon: Don't know how to lift " <> show con

liftDec :: Dec -> Q [Dec]
liftDec (DataD _ tyName tyVarBndrs _ cons _)
    | null tyVarBndrs    = fail $ "Type " <> show tyName <> " needs at least one free variable"
    | otherwise          = do
        let nextTyName = tyVarBndrName (last tyVarBndrs)
        mconcat <$> forM cons (liftCon (ConT tyName) nextTyName)
liftDec dec = fail $ "liftDec: Don't know how to lift " <> show dec

-- | Genarate definitions for DSL operations expressed by the data type
mkProgramOps :: Name -> Q [Dec]
mkProgramOps typCon = do
    typInfo <- reify typCon
    case typInfo of
      TyConI dec -> liftDec dec
      otherwise  -> fail "mkProgramOps expects a type constructor"
