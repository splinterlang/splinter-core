> module Language.Splinter.Parser.ModuleDesc where

> import Control.Monad

> import Text.Parsec
> import Text.Parsec.Indent

> import Language.Splinter.Parser.ParserType
> import Language.Splinter.Parser.Lexer

Modules look like this:

 module Foo:
   imports Bar, Baz
   exports foo


> data ModuleDesc = ModuleDesc
>   { pos         :: SourcePos
>   , moduleName  :: ModuleName
>   , moduleBlock :: PropertyList
>   } deriving (Show)

> type ModuleName = String

> type PropertyList = [ModuleProperty]

> data ModuleProperty = ImportProperty ImportList
>                     | ExportProperty ExportList
>   deriving(Show)

> type ImportList = [Import]

> data Import = Import
>   { importName   :: ImportName
>   , importIdents :: IdentList
>   , qalName      :: Maybe (String)
>   } deriving (Show)

> type ImportName = String

> type IdentList = [String]

> type ExportList = [Export]

> data Export = Export
>   { exportName   :: ExportName
>   , exportIdents :: ExportIdents
>   } deriving (Show)

> type ExportName = String

> data ExportIdents = ExportAll | ExportSome FieldList
>   deriving (Show)

> type FieldList = [Field]

> type Field = String

> aModuleDesc :: EParser ModuleDesc
> aModuleDesc = do
>   pos <- getPosition
>   reserved "module"
>   name <- upperIdent
>   _ <- lexeme $ char ':'
>   props <- block aModuleProperty
> 
>   return $ ModuleDesc pos name props

> aModuleProperty :: EParser ModuleProperty
> aModuleProperty = ip <|> ep
>   where
>     ip = liftM ImportProperty anImportList
>     ep = liftM ExportProperty anExportList

> anImportList :: EParser ImportList
> anImportList = do
>   reserved "imports"
>   sepBy anImport comma

> anImport :: EParser Import
> anImport = do
>   pos <- getPosition
>   ident <- identifier
>   identList <- parenIdents
>   qualName <- optionMaybe aQualName
> 
>   return $ Import ident identList qualName

> aQualName = do
>   reserved "as"
>   ident <- upperIdent
>   return ident

> anExportList :: EParser ExportList
> anExportList = do
>   reserved "exports"
>   sepBy anExport comma
 
> anExport :: EParser Export
> anExport  = do
>   pos <- getPosition
>   ident <- identifier
>   identList <- parseAllOrSome
>   
>   return $ Export ident identList

> parseAllOrSome = (try pAll) <|> pSome
>   where
>     pAll = (parens $ string "..") >> return ExportAll
>     pSome = do
>       idts <- parenIdents
>       return $ ExportSome idts

> parenIdents :: EParser [String]
> parenIdents = do
>   identList <- optionMaybe $ parens $ sepBy identifier comma 
>   
>   return $ case identList of
>               Nothing -> []
>               Just l -> l
