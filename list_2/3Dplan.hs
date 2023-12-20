data Mov = Z Double | X Double | Y Double
                  deriving (Eq, Show, Read)

type Point3D = (Double, Double, Double)

type Object3D = [Point3D]

transladaObjeto :: Object3D -> [Mov] -> Object3D

rotacionaObjeto :: Double -> Double -> Double -> Object3D -> Maybe Object3D

toRad :: Double -> Double

main = do
       coord <- getLine
       mov <- getLine
       angX <- getLine
       angY <- getLine
       angZ <- getLine
       let transladado = transladaObjeto (read coord :: Object3D) (read mov :: [Mov])
       let rotacionado = rotacionaObjeto (read angX) (read angY) (read angZ) transladado
       print rotacionado