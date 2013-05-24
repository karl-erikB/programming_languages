package org.grash

import groovy.io.FileType
import java.util.regex.Matcher

import static Tokens.*

/* We keep a single symbol table during the entire execution. */
final symbols = new Symbols()

/* The script is read from this file. */
def infile = System.in

/* Primitive command line processing: -i:infile specifies a file to read from.
 * All other parameters are interpreted as symbol:value forward definitions. */
args.each() { arg ->
    def pair = arg.tokenize ':'
    assert pair.size == 2
    if (pair[0] == "-i") {
        infile = new File(pair[1])
    } else {
        symbols.addForwardDefinition pair[0], pair[1]
    }
}

/* Our main interpreter loop. Grash is line based, so we can simply process each
 * line one by one. */
def exitCode = 0
infile.eachLine() { line, nr ->
    try {
        switch (line) {

        /* Parameter declaration. */
        case ~/par$WHITE($SYM)/:
            symbols.addDeclaration Matcher.lastMatcher[0][1]
            break

        /* Parameter copy/replace definition. */
        case ~/defp$WHITE($SYM)$WHITE($SYM):($REGEXP):($VAL)/:
            final sym = Matcher.lastMatcher[0][1]
            final val = symbols.getSymbol Matcher.lastMatcher[0][2]
            final pattern = Matcher.lastMatcher[0][3]
            final replace = Matcher.lastMatcher[0][4]
            symbols.addDefinition sym, (val =~ pattern).replaceAll(replace)
            break

        /* Parameter file pattern matching definition. */
        case ~/deff$WHITE($SYM)$WHITE($PATH):($REGEXP)/:
            final sym = Matcher.lastMatcher[0][1]
            final path = Matcher.lastMatcher[0][2]
            final pattern = Matcher.lastMatcher[0][3]

            def files = []
            final dir = new File(path)
            dir.eachFile(FileType.FILES) { file ->
                final name = file.getName()
                if (name =~ pattern) {
                    files << name
                }
            }

            symbols.addDefinition sym, files.join(" ")
            break

        /* Standard parameter definition. */
        case ~/def$WHITE($SYM)$WHITE($VAL)/:
            symbols.addDefinition Matcher.lastMatcher[0][1],
                                  Matcher.lastMatcher[0][2]
            break

        /* All actions are handled within the interpreter. Definitions
         * and declarations are not supported within actions. */
        case ~ACTION:
            final interpreter = new Interpreter(symbols)
            exitCode = interpreter.run line, []
            break

        /* Whitespace and comments are skipped. */
        case ~WHITE_:
        case ~COMMENT:
            break
        default: println "Illegal statement on line $nr: $line"; break
        }
    } catch (GrashException e) {
        println "Error on line $nr: $e"
    }
}

/* Groovy seems to need System.exit() to return a specific error code. */
System.exit(exitCode)
