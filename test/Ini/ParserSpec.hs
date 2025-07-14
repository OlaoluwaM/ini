module Ini.ParserSpec where

import Ini.Parser
import Test.Hspec

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Data.Attoparsec.Text (parseOnly)
import Data.Either (isLeft)

spec :: Spec
spec = do
    describe "Header parsing" $ do
        it "parses simple header" $ do
            parseOnly parseHeader "[database]" `shouldBe` Right (Header "database")

        it "parses header with alphanumeric characters" $ do
            parseOnly parseHeader "[user123]" `shouldBe` Right (Header "user123")

        it "parses header with dots" $ do
            parseOnly parseHeader "[section.subsection]" `shouldBe` Right (Header "section.subsection")
            parseOnly parseHeader "[.subsection]" `shouldBe` Right (Header ".subsection")

        it "fails on unclosed header" $ do
            parseOnly parseHeader "[database" `shouldSatisfy` isLeft

        it "fails on unopened header" $ do
            parseOnly parseHeader "database]" `shouldSatisfy` isLeft

    describe "Assignment parsing" $ do
        it "parses simple assignment" $ do
            parseOnly parseAssignment "key=value" `shouldBe` Right (Assignment "key" "value")

        it "parses assignment with spaces in value" $ do
            parseOnly parseAssignment "name=John Doe" `shouldBe` Right (Assignment "name" "John Doe")

        it "parses assignment with numeric value" $ do
            parseOnly parseAssignment "port=8080" `shouldBe` Right (Assignment "port" "8080")

        it "parses assignment with boolean value" $ do
            parseOnly parseAssignment "debug=true" `shouldBe` Right (Assignment "debug" "true")

        it "parses assignment with empty value" $ do
            parseOnly parseAssignment "empty=" `shouldBe` Right (Assignment "empty" "")

        it "fails on assignment without equals" $ do
            parseOnly parseAssignment "key value" `shouldSatisfy` isLeft

        it "parses an assignment with a newline in its value" $ do
            parseOnly parseAssignment "key=val\nue" `shouldBe` Right (Assignment{key = "key", value = "val"})

    describe "Section parsing" $ do
        it "parses section with one assignment" $ do
            let input = "[user]\nname=John"
            case parseOnly parseSection input of
                Right (Section (header, assignments)) -> do
                    header `shouldBe` Header "user"
                    assignments `shouldBe` [Assignment "name" "John"]
                Left _ -> expectationFailure "Should parse successfully"

        it "parses section with multiple assignments" $ do
            let input = "[database]\nhost=localhost\nport=5432\nuser=admin"
            case parseOnly parseSection input of
                Right (Section (header, assignments)) -> do
                    header `shouldBe` Header "database"
                    assignments `shouldMatchList` [Assignment "host" "localhost", Assignment "port" "5432", Assignment "user" "admin"]
                Left _ -> expectationFailure "Should parse successfully"

        it "handles sections with different numbers of assignments" $ do
            let input = "[one]\nkey=value\n\n[two]\nkey1=value1\nkey2=value2\n\n[three]\na=1\nb=2\nc=3\n"
            case parseOnly parseIniConfig input of
                Right (IniConfig config) -> do
                    length config `shouldBe` 3
                Left _ -> expectationFailure "Should parse sections with different assignment counts"

    describe "Comment parsing" $ do
        it "parses comment with semicolon" $ do
            parseOnly skipComment "; This is a comment" `shouldBe` Right ()

        it "parses comment with hash" $ do
            parseOnly skipComment "# This is a comment" `shouldBe` Right ()

        it "fails on line without comment marker" $ do
            parseOnly skipComment "This is not a comment" `shouldSatisfy` isLeft

        it "handles semicolon comments" $ do
            parseOnly skipComment "; This is a comment with special chars!@#$%^&*()" `shouldBe` Right ()

        it "handles hash comments" $ do
            parseOnly skipComment "# Another comment with numbers 123 and symbols" `shouldBe` Right ()

        it "handles empty comments" $ do
            parseOnly skipComment ";" `shouldBe` Right ()
            parseOnly skipComment "#" `shouldBe` Right ()

    describe "Edge cases" $ do
        describe "Empty and whitespace handling" $ do
            it "handles empty input" $ do
                parseOnly parseIniConfig "" `shouldBe` Right emptyConfig

            it "handles whitespace-only input" $ do
                parseOnly parseIniConfig "   \n\n  " `shouldBe` Right emptyConfig

            it "handles assignment with empty value" $ do
                parseOnly parseAssignment "key=" `shouldBe` Right (Assignment "key" "")

            it "handles section with empty assignments" $ do
                parseOnly parseSection "[empty]\nkey=" `shouldBe` Right (Section (Header "empty", [Assignment "key" ""]))

        describe "Special characters" $ do
            it "handles dots in keys" $ do
                parseOnly parseAssignment "config.key=value" `shouldBe` Right (Assignment "config.key" "value")

            it "handles spaces in values" $ do
                parseOnly parseAssignment "name=John Doe Smith" `shouldBe` Right (Assignment "name" "John Doe Smith")

            it "handles special characters in values" $ do
                parseOnly parseAssignment "path=/usr/local/bin" `shouldBe` Right (Assignment "path" "/usr/local/bin")

            it "handles URLs in values" $ do
                parseOnly parseAssignment "url=https://example.com/path?query=value" `shouldBe` Right (Assignment "url" "https://example.com/path?query=value")

        describe "Malformed input" $ do
            it "rejects assignment with newline in key" $ do
                parseOnly parseAssignment "key\nname=value" `shouldSatisfy` isLeft

            it "rejects section without header" $ do
                parseOnly parseSection "key=value" `shouldSatisfy` isLeft

        describe "Boundary conditions" $ do
            it "handles very long header names" $ do
                let longHeader = "[" <> T.replicate 1000 "a" <> "]"
                parseOnly parseHeader longHeader `shouldBe` Right (Header (T.replicate 1000 "a"))

            it "handles very long values" $ do
                let longValue = "key=" <> T.replicate 1000 "x"
                parseOnly parseAssignment longValue `shouldBe` Right (Assignment "key" (T.replicate 1000 "x"))

            it "handles single character components" $ do
                parseOnly parseHeader "[a]" `shouldBe` Right (Header "a")
                parseOnly parseAssignment "a=b" `shouldBe` Right (Assignment "a" "b")

emptyConfig :: IniConfig
emptyConfig = IniConfig Map.empty
