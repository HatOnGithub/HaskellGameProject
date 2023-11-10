module FileLocations where

animLocations :: [(String, String)]
animLocations = [
      ("Mario" , "src\\Textures\\Mario")
    , ("Goomba", "src\\Textures\\Enemies\\Goomba")

    , ("Brick" , "src\\Textures\\Blocks\\Brick" )
    , ("Pipe"  , "src\\Textures\\Blocks\\Pipe"  )
    , ("Block" , "src\\Textures\\Blocks\\Block" )
    , ("Grass" , "src\\Textures\\Blocks\\Grass" )
    , ("Dirt"  , "src\\Textures\\Blocks\\Dirt"  )
    , ("QBlock", "src\\Textures\\Blocks\\QBlock")
    , ("Castle", "src\\Textures\\Blocks\\Castle")
    , ("Pole"  , "src\\Textures\\Blocks\\Pole"  )
    , ("Flag"  , "src\\Textures\\Blocks\\Flag"  )
    , ("FakeBrick" , "src\\Textures\\Blocks\\FakeBrick" )
    ]


levels :: [(String, String)]
levels = [("World 1-1", "src\\Worlds\\1-1.txt")]
