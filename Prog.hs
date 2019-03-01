-- Autor(es): Joaquin Ruiz , Sebastian del Arca
-- Numero(s) de estudiante(s): 206164 , 202096
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Prog where

-- Tipos
type Var = String

data Exp where {
    V    :: Var -> Exp;
    N  :: Int -> Exp;
    (:+)  :: Exp -> Exp -> Exp;
    (:-)  :: Exp -> Exp -> Exp;
    (:*) :: Exp -> Exp -> Exp  }
  deriving(Eq,Show)

-- Se define que :* tiene mayor precedencia que :+ y :-
infixl 6 :+
infixl 6 :-
infixl 8 :*

type Memoria = [(Var,Int)]

data Prog where {
    (:=)  :: Var -> Exp -> Prog;
    (:>)  :: Prog -> Prog -> Prog;
    If    :: Exp -> Prog -> Prog -> Prog;
    While :: Exp -> Prog -> Prog
	
     	}
  deriving(Eq,Show)

-- Se define que := tiene mayor precedencia que :>
infixr 5 :=
infixr 3 :>

-- 1
primeroDelPar :: (Var,Int) -> Var
primeroDelPar = \p -> case p of {
			  (x,n) -> x; 

}
segundoDelPar :: (Var,Int) -> Int
segundoDelPar = \p -> case p of {
			  (x,n) -> n; 

}

esFun :: (Eq a) => [(a,b)] -> Bool
esFun = \parL -> case parL of {
           [] -> False;
		   x:xs -> case x of {
		        (izq,der) -> case xs of {
				          [] -> True;
						  y:ys -> case izq == primeroDelpar y of {
						          True -> case segundoDelPar y == der of {
								                  True -> False;
												  False -> esFun xs;
								  
								  };
						  
						  };
				};
		   
		   };
} 


(@@) :: Var -> Memoria -> Int
(@@) = \v -> \m -> case m of {
              [] -> error "Variable no inicializada";
			  x:xs -> case (primeroDelPar x) == v of {
			           True -> segundoDelPar x;
					   False -> (@@) v xs;
			  };

}
sacarVarIgual :: Memoria -> (Var,Int) -> Memoria
sacarVarIgual = \m -> \p -> case m of {
                   [] -> [];
				   x:xs -> case  (primeroDelPar x) == (primeroDelPar p) of{
				                 True ->  p : sacarVarIgual xs p;
								 False ->  x : sacarVarIgual xs p;
								 
				   
				   }; 

}


-- 2

upd :: (Var,Int) -> Memoria -> Memoria
upd = \p -> \m -> case m of {
             [] -> p : m ;
			 x:xs -> case (primeroDelPar x) == (primeroDelPar p) of {
			       True -> sacarVarIgual m p;
				   False -> p : m;
			 
			 };

}


-- 3

eval :: Exp -> Memoria -> Int
eval = \e -> \m -> case e of {
             V string -> case m of {
			        [] -> error "No hay variables";
					x:xs -> (@@) string m;
			 };
			 N numero -> numero;

             exp1 :+ exp2  ->  (eval exp1 m ) + (eval exp2 m);
             exp1 :- exp2  ->  (eval exp1 m) - (eval exp2 m);
             exp1 :* exp2  -> (eval exp1 m ) * (eval exp2 m);			 
			
}



			 
              




-- 4


run :: Prog -> Memoria -> Memoria 
run = \p -> \m -> case p of {
         --llama a upd que agrega o midifica el valor asignado a la memorio.
        variable := expresion ->  upd (variable, eval expresion m) m  ;
		--primero corre p1 que devuelve la memorio con p1 ya corrido y despues corre p2.
		p1 :> p2 -> run p2 ( run p1 m);
		
		If expresion p1 p2 -> case  eval expresion m of {
		                     0 -> run p2 m;
							 _ -> run p1 m;
		
		          };
				  While expresion p -> case eval expresion m of {
				                        0 -> m ;
										_ ->  run (p :> While expresion p) m  ;
			  
				  };
		
}


-- Ejemplos
p0 :: Prog
p0 = "x" := N 1 :> "x" :=  V "x" :+ N 10
-- resultado = 11
p1 :: Prog
p1 =  "x" := N 1 :> "y" := N 2 :>
      If  (V "y" :- V "x")
      ("z" := N 10)
      ("z" := N 20)

p2 :: Prog
p2 = "x" := N 10 :> "y" := N 5 :> 
     While (V "x") ( "y" := V "y" :+ N 2 :> "x" :=  V "x" :-N 1)

-- 5

{-- la variable aux guarda el valor de x , despues x vale y e y vale aux (El valor de x)--}
swap:: Prog
swap = "aux" := V "x" :> "x" := V "y" :> "y" := V "aux"



-- 6
{--
"x" := N 10 :> "y" := N 5 :> 
     While (V "x") ( "y" := V "y" :+ N 2 :> "x" :=  V "x" :-N 1)
--}
fact :: Int -> Prog
fact = \n -> ("x" := N n) :> 
			("y" := N 1) :> 
			While (V "x") 
				("y" := (V "y" :* V "x") :>  
				"x" := (V "x" :- N 1))



