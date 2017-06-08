module Calm.FS exposing (..)


type RelativeFile
    = RelativeFile String


type AbsoluteFile
    = AbsoluteFile String


type FileName
    = AbsoluteFileName AbsoluteFile
    | RelativeFileName RelativeFile


type RelativeDir
    = RelativeDir String


type AbsoluteDir
    = AbsoluteDir String


type DirName
    = AbsoluteDirName AbsoluteDir
    | RelativeDirName RelativeDir
