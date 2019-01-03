data Natural = Cero | Suc Natural deriving (Eq, Show)

data ListaDeNaturales = Nil | Cons Natural ListaDeNaturales deriving (Eq, Show)

mayorQue :: Natural -> Natural -> Bool
mayorQue n m = case n of
                 Cero -> False
                 Suc a -> case m of
                            Cero -> True
                            Suc b -> mayorQue a b
    
menorQue :: Natural -> Natural -> Bool
menorQue n m = case m of
                 Cero -> False
                 Suc b -> case n of
                            Cero -> True
                            Suc a -> menorQue a b

igual :: Natural -> Natural -> Bool
igual n m = case n of
              Cero -> case m of
                        Cero -> True
                        Suc _ -> False
                        Suc a -> case m of
                                   Cero -> False
                                   Suc b -> igual a b

concate :: ListaDeNaturales -> ListaDeNaturales -> ListaDeNaturales
concate l1 l2 = case l1 of
                  Nil -> l2
                  Cons n l -> Cons n (concate l l2)

reversa :: ListaDeNaturales -> ListaDeNaturales
reversa l = case l of
              Nil -> Nil
              Cons n ls -> concate (reversa ls) (Cons n Nil)


type Indice = Int

data PL = Top | Bot  | Var Indice
              | Oneg PL 
              | Oand PL PL | Oor PL PL 
              | Oimp PL PL deriving (Eq, Show)

conj :: PL -> [PL]
conj phi = case phi of
             Top -> []
             Bot -> []
             Var _ -> []
             Oneg alpha -> conj phi
             Oand alpha beta -> [Oand alpha beta] ++ conj alpha ++ conj beta
             Oor alpha beta -> conj alpha ++ conj beta
             Oimp alpha beta -> conj alpha ++ conj beta

numConj :: PL -> Int
numConj phi = case phi of
                Top -> 0
                Bot -> 0
                Var _ -> 0
                Oneg alpha -> numConj alpha
                Oand alpha beta -> 1 + numConj alpha + numConj beta
                Oor alpha beta -> numConj alpha + numConj beta
                Oimp alpha beta -> numConj alpha + numConj beta


type Modelo = [Indice]
type Valuacion = Indice -> Bool

satMod :: Modelo -> PL -> Bool
satMod m phi = case phi of
                 Top -> True
                 Bot -> False
                 Var n -> elem n m
                 Oneg alpha -> not (satMod m alpha)
                 Oand alpha beta -> (satMod m alpha) && (satMod m beta)
                 Oor alpha beta -> (satMod m alpha) || (satMod m beta)
                 Oimp alpha beta -> not (satMod m alpha) || (satMod m beta)


modeloToValuacion :: Modelo -> Valuacion
modeloToValuacion m = sigma_m
  where
    sigma_m :: Valuacion
    sigma_m v = elem v m
  


satPL :: Valuacion -> PL -> Bool
satPL valor phi = case phi of
                    Top -> True
                    Bot -> False
                    Var n -> valor n
                    Oneg alpha -> not (satPL valor alpha)
                    Oand alpha beta -> (satPL valor alpha) && (satPL valor beta)
                    Oor alpha beta -> (satPL valor alpha) || (satPL valor beta)
                    Oimp alpha beta -> not (satPL valor alpha) || (satPL valor beta)


esClausula :: PL -> Bool
esClausula phi = case phi of
                   Bot -> True
                   Var _ -> True
                   Oneg alpha -> case alpha of
                                   Var _ -> True
                                   _ -> False
                                   Oor alpha beta -> esClausula alpha && esClausula beta
                                   _ -> False

esCNF :: PL -> Bool
esCNF phi = case phi of
              Oand alpha beta -> esCNF alpha && esCNF beta
              _ -> esClausula phi

esTermino :: PL -> Bool
esTermino phi = case phi of
                  Top -> True
                  Var _ -> True
                  Oneg alpha -> case alpha of
                                  Var _ -> True
                                  _ -> False
                                  Oand alpha beta -> esTermino alpha && esTermino beta
                                  _ -> False
     
esDNF :: PL -> Bool
esDNF phi = case phi of
              Oor alpha beta -> esDNF alpha && esDNF beta
              _ -> esTermino phi


quitaImp :: PL -> PL
quitaImp phi = case phi of
                 Top -> Top
                 Bot -> Bot
                 Var x -> Var x
                 Oneg x -> Oneg (quitaImp x)
                 Oand x y -> Oand (quitaImp x) (quitaImp y)
                 Oor x y -> Oor (quitaImp x) (quitaImp y)
                 Oimp x y -> Oor (quitaImp (Oneg  x)) (quitaImp y)


noImpNNF :: PL -> PL
noImpNNF phi = case phi of
                 Top -> phi
                 Bot -> phi
                 Var v -> Var v
                 Oneg alfa -> case alfa of
                                Top -> Bot
                                Bot -> Top
                                Var v -> Oneg (Var v)
                                Oneg g -> noImpNNF g
                                Oand g h -> noImpNNF (Oor (Oneg g) (Oneg h))
                                Oor g h -> noImpNNF (Oand (Oneg g) (Oneg h))
                                Oand alfa beta -> Oand (noImpNNF alfa) (noImpNNF beta)
                                Oor alfa beta -> Oor (noImpNNF alfa) (noImpNNF beta)

toNNF :: PL -> PL
toNNF = noImpNNF . quitaImp


distr :: PL -> PL -> PL
distr phi gam = case (phi,gam) of
                  (Oand alpha beta,_) -> Oand (distr alpha gam) (distr beta gam)
                  (_,Oand alpha beta) -> Oand (distr phi alpha) (distr phi beta)
                  (_,_) -> Oor phi gam

toCNF :: PL -> PL
toCNF phi = case phi of
              Top -> Top
              Bot -> Bot
              Var n -> Var n
              Oneg alpha -> Oneg (toCNF alpha)
              Oand alpha beta -> Oand (toCNF alpha) (toCNF beta)
              Oor alpha beta -> distr (toCNF alpha) (toCNF beta) 


cnf :: PL -> PL
cnf = toCNF . toNNF
