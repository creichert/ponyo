structure Test =
struct
    local
        structure FileSystem = Ponyo.Os.FileSystem
        structure Sml = Ponyo.Sml
        structure String = Ponyo.String
        structure StringMap = Ponyo_Container_Map (String)
    in

    (* Maps files to a list of test functions. *)
    val tests : string list StringMap.t ref = ref StringMap.empty

    val testSignatureSuffix = "_test.ML"

    fun discoverFileTests (path: string, ast: Sml.Ast.t, fileTests: string list) : string list =
        case ast of
            Sml.Ast.Signature (name, body) =>
              let val fileTests = discoverFileTests (path, body, []) in
                  tests := StringMap.insert tests (path, fileTests);
                  []
              end
          | Sml.Ast.SignatureBody (children) =>
              map (fn child => discoverFileTests (path, child, test)) children
          | Sml.Ast.ValueDec (name, (Ast.Type ty)) => [name]
          | _ => []

    fun discoverTests (directory: string) : unit =
        let
            fun parse (path) =
                Sml.parseFile (Path.join [directory, path])
        in
            FileSystem.walk directory (fn (path) =>
                if String.hasSuffix (path, testSignatureSuffix)
                    then ignore (discoverFileTests (parse path, []))
                else ()
            )
        end

    fun runTests(directory: string) : unit =
        let
        
        in
        end
end

structure Main =
struct
    local
        structure Cli = Ponyo.Os.Cli

        structure String = Ponyo.String
        structure Format = Ponyo.Format
    in

    val testDirectoryFlag = Cli.Flag.Named ("d", "test-directory")

    val spec =
        let
            open Cli

            val testDirectoryDesc = "directory searched recursively for tests"
        in
            ("ponyo-test",
             "Ponyo-test is a tool for testing Standard ML programs.",
             [],
             [(testDirectoryFlag, Arg.optional (Arg.string, ""))])
        end

    fun main () =
        let
            val args = Cli.getArgs (spec) handle
                Fail reason =>
                    (Format.printf "ERROR: %\n\n" [reason]; Cli.doHelp (spec); [])
            val [testDirectory] = Cli.getNamed (args, testDirectoryFlag)
        in
            Test.runTests (testDirectory)
        end
end

val main = Main.main
