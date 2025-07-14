module Ini.SampleDataSpec where

import Ini.Parser
import Test.Hspec

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO

import Data.Attoparsec.Text (parseOnly)
import Data.List (find)
import System.FilePath ((</>))

spec :: Spec
spec = do
    describe "Config file parsing" $ do
        it "parses config.ini successfully" $ do
            content <- TIO.readFile ("test" </> "data" </> "config.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    Map.size config `shouldBe` 4
                    Map.keys config `shouldContain` [Header "database"]
                    Map.keys config `shouldContain` [Header "server"]
                    Map.keys config `shouldContain` [Header "logging"]
                    Map.keys config `shouldContain` [Header "cache"]
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

        it "parses settings.ini successfully" $ do
            content <- TIO.readFile ("test" </> "data" </> "settings.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    Map.size config `shouldBe` 4
                    Map.keys config `shouldContain` [Header "user"]
                    Map.keys config `shouldContain` [Header "preferences"]
                    Map.keys config `shouldContain` [Header "display"]
                    Map.keys config `shouldContain` [Header "shortcuts"]
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

        it "parses game.ini successfully" $ do
            content <- TIO.readFile ("test" </> "data" </> "game.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    Map.size config `shouldBe` 4
                    Map.keys config `shouldContain` [Header "graphics"]
                    Map.keys config `shouldContain` [Header "audio"]
                    Map.keys config `shouldContain` [Header "controls"]
                    Map.keys config `shouldContain` [Header "gameplay"]
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

        it "parses environment.ini successfully" $ do
            content <- TIO.readFile ("test" </> "data" </> "environment.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    Map.size config `shouldBe` 5
                    Map.keys config `shouldContain` [Header "development"]
                    Map.keys config `shouldContain` [Header "production"]
                    Map.keys config `shouldContain` [Header "testing"]
                    Map.keys config `shouldContain` [Header "build"]
                    Map.keys config `shouldContain` [Header "dependencies"]
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

        it "parses email.ini successfully" $ do
            content <- TIO.readFile ("test" </> "data" </> "email.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    Map.size config `shouldBe` 4
                    Map.keys config `shouldContain` [Header "smtp"]
                    Map.keys config `shouldContain` [Header "email_templates"]
                    Map.keys config `shouldContain` [Header "limits"]
                    Map.keys config `shouldContain` [Header "settings"]
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

        it "parses test_data.ini successfully" $ do
            content <- TIO.readFile ("test" </> "data" </> "test_data.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    Map.size config `shouldBe` 6
                    Map.keys config `shouldContain` [Header "strings"]
                    Map.keys config `shouldContain` [Header "numbers"]
                    Map.keys config `shouldContain` [Header "booleans"]
                    Map.keys config `shouldContain` [Header "arrays"]
                    Map.keys config `shouldContain` [Header "special_characters"]
                    Map.keys config `shouldContain` [Header "multiline"]
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

    describe "Specific value parsing" $ do
        it "extracts correct values from config.ini" $ do
            content <- TIO.readFile ("test" </> "data" </> "config.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    case Map.lookup (Header "database") config of
                        Just assignments -> do
                            let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                            findValue "host" `shouldBe` Just "localhost"
                            findValue "port" `shouldBe` Just "5432"
                            findValue "username" `shouldBe` Just "admin"
                        Nothing -> expectationFailure "Database section should exist"
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

        it "extracts correct values from settings.ini" $ do
            content <- TIO.readFile ("test" </> "data" </> "settings.ini")
            case parseOnly parseIniConfig content of
                Right (IniConfig config) -> do
                    case Map.lookup (Header "user") config of
                        Just assignments -> do
                            let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                            findValue "name" `shouldBe` Just "John Doe"
                            findValue "email" `shouldBe` Just "john.doe@example.com"
                            findValue "is_admin" `shouldBe` Just "false"
                        Nothing -> expectationFailure "User section should exist"
                Left err -> expectationFailure $ "Should parse successfully: " ++ err

    describe "Sample data validation" $ do
        describe "config.ini structure" $ do
            it "has correct sections and key-value pairs" $ do
                content <- TIO.readFile ("test" </> "data" </> "config.ini")
                case parseOnly parseIniConfig content of
                    Right (IniConfig config) -> do
                        -- Check database section
                        case Map.lookup (Header "database") config of
                            Just assignments -> do
                                let keys = map (.key) assignments
                                keys `shouldContain` ["host", "port", "username", "password", "database_name", "connection_timeout"]
                            Nothing -> expectationFailure "Database section missing"

                        -- Check server section
                        case Map.lookup (Header "server") config of
                            Just assignments -> do
                                let keys = map (.key) assignments
                                keys `shouldContain` ["host", "port", "debug", "max_connections"]
                            Nothing -> expectationFailure "Server section missing"

                        -- Check logging section
                        case Map.lookup (Header "logging") config of
                            Just assignments -> do
                                let keys = map (.key) assignments
                                keys `shouldContain` ["level", "file", "max_size", "backup_count"]
                            Nothing -> expectationFailure "Logging section missing"

                        -- Check cache section
                        case Map.lookup (Header "cache") config of
                            Just assignments -> do
                                let keys = map (.key) assignments
                                keys `shouldContain` ["enabled", "type", "host", "port", "ttl"]
                            Nothing -> expectationFailure "Cache section missing"
                    Left err -> expectationFailure $ "Should parse successfully: " ++ err

        describe "settings.ini structure" $ do
            it "has correct user preferences structure" $ do
                content <- TIO.readFile ("test" </> "data" </> "settings.ini")
                case parseOnly parseIniConfig content of
                    Right (IniConfig config) -> do
                        -- Check user section
                        case Map.lookup (Header "user") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "name" `shouldBe` Just "John Doe"
                                findValue "email" `shouldBe` Just "john.doe@example.com"
                                findValue "is_admin" `shouldBe` Just "false"
                            Nothing -> expectationFailure "User section missing"

                        -- Check preferences section
                        case Map.lookup (Header "preferences") config of
                            Just assignments -> do
                                let keys = map (.key) assignments
                                keys `shouldContain` ["theme", "language", "timezone", "notifications"]
                            Nothing -> expectationFailure "Preferences section missing"
                    Left err -> expectationFailure $ "Should parse successfully: " ++ err

        describe "game.ini structure" $ do
            it "has correct game configuration structure" $ do
                content <- TIO.readFile ("test" </> "data" </> "game.ini")
                case parseOnly parseIniConfig content of
                    Right (IniConfig config) -> do
                        -- Check graphics section
                        case Map.lookup (Header "graphics") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "resolution" `shouldBe` Just "1920x1080"
                                findValue "vsync" `shouldBe` Just "true"
                                findValue "texture_quality" `shouldBe` Just "high"
                            Nothing -> expectationFailure "Graphics section missing"

                        -- Check audio section
                        case Map.lookup (Header "audio") config of
                            Just assignments -> do
                                let keys = map (.key) assignments
                                keys `shouldContain` ["master_volume", "music_volume", "effects_volume", "voice_volume"]
                            Nothing -> expectationFailure "Audio section missing"

                        -- Check controls section
                        case Map.lookup (Header "controls") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "key_forward" `shouldBe` Just "W"
                                findValue "key_backward" `shouldBe` Just "S"
                                findValue "key_jump" `shouldBe` Just "Space"
                            Nothing -> expectationFailure "Controls section missing"
                    Left err -> expectationFailure $ "Should parse successfully: " ++ err

        describe "test_data.ini data types" $ do
            it "handles various data types correctly" $ do
                content <- TIO.readFile ("test" </> "data" </> "test_data.ini")
                case parseOnly parseIniConfig content of
                    Right (IniConfig config) -> do
                        -- Check strings section
                        case Map.lookup (Header "strings") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "simple_string" `shouldBe` Just "Hello World"
                                findValue "quoted_string" `shouldBe` Just "\"This is a quoted string\""
                            Nothing -> expectationFailure "Strings section missing"

                        -- Check numbers section
                        case Map.lookup (Header "numbers") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "integer" `shouldBe` Just "42"
                                findValue "negative_integer" `shouldBe` Just "-10"
                                findValue "float" `shouldBe` Just "3.14159"
                            Nothing -> expectationFailure "Numbers section missing"

                        -- Check booleans section
                        case Map.lookup (Header "booleans") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "true_value" `shouldBe` Just "true"
                                findValue "false_value" `shouldBe` Just "false"
                                findValue "yes_value" `shouldBe` Just "yes"
                                findValue "no_value" `shouldBe` Just "no"
                            Nothing -> expectationFailure "Booleans section missing"

                        -- Check special characters section
                        case Map.lookup (Header "special_characters") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "url" `shouldBe` Just "https://example.com/path?query=value"
                                findValue "email" `shouldBe` Just "test@example.com"
                            Nothing -> expectationFailure "Special characters section missing"
                    Left err -> expectationFailure $ "Should parse successfully: " ++ err

        describe "email.ini email configuration" $ do
            it "has correct email configuration structure" $ do
                content <- TIO.readFile ("test" </> "data" </> "email.ini")
                case parseOnly parseIniConfig content of
                    Right (IniConfig config) -> do
                        -- Check SMTP section
                        case Map.lookup (Header "smtp") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "server" `shouldBe` Just "smtp.gmail.com"
                                findValue "port" `shouldBe` Just "587"
                                findValue "use_tls" `shouldBe` Just "true"
                            Nothing -> expectationFailure "SMTP section missing"

                        -- Check limits section
                        case Map.lookup (Header "limits") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "max_recipients" `shouldBe` Just "100"
                                findValue "daily_limit" `shouldBe` Just "1000"
                            Nothing -> expectationFailure "Limits section missing"
                    Left err -> expectationFailure $ "Should parse successfully: " ++ err

        describe "environment.ini deployment configuration" $ do
            it "has correct environment configuration structure" $ do
                content <- TIO.readFile ("test" </> "data" </> "environment.ini")
                case parseOnly parseIniConfig content of
                    Right (IniConfig config) -> do
                        -- Check development section
                        case Map.lookup (Header "development") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "debug" `shouldBe` Just "true"
                                findValue "port" `shouldBe` Just "3000"
                                findValue "hot_reload" `shouldBe` Just "true"
                            Nothing -> expectationFailure "Development section missing"

                        -- Check production section
                        case Map.lookup (Header "production") config of
                            Just assignments -> do
                                let findValue key = (.value) <$> find (\a -> a.key == key) assignments
                                findValue "debug" `shouldBe` Just "false"
                                findValue "port" `shouldBe` Just "80"
                                findValue "ssl" `shouldBe` Just "true"
                            Nothing -> expectationFailure "Production section missing"
                    Left err -> expectationFailure $ "Should parse successfully: " ++ err
