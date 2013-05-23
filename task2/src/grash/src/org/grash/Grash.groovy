package org.grash

import groovy.io.FileType
import java.util.regex.Matcher

import static Tokens.*

final symbols = new Symbols()

args.each() { arg ->
    def pair = arg.tokenize ':'
    assert pair.size == 2
    symbols.addForwardDefinition pair[0], pair[1]
}

System.in.eachLine() { line, nr ->
    try {
        switch (line) {
        case ~/par$WHITE($SYM)/:
            symbols.addDeclaration Matcher.lastMatcher[0][1]
            break
        case ~/defp$WHITE($SYM)$WHITE($SYM):($REGEXP):($VAL)/:
            final sym = Matcher.lastMatcher[0][1]
            final val = symbols.getSymbol Matcher.lastMatcher[0][2]
            final pattern = Matcher.lastMatcher[0][3]
            final replace = Matcher.lastMatcher[0][4]
            symbols.addDefinition sym, (val =~ pattern).replaceAll(replace)
            break
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
        case ~/def$WHITE($SYM)$WHITE($VAL)/:
            symbols.addDefinition Matcher.lastMatcher[0][1],
                                  Matcher.lastMatcher[0][2]
            break
        case ~/prm\(.*\)/: println('prm'); break
        case ~/seq\(.*\)/: println('seq'); break
        case ~/alt\(.*\)/: println('alt'); break
        case ~/set\(.*\)/: println('set'); break
        default: println "Illegal statement on line $nr: $line"; break
        }
    } catch (GrashException e) {
        println "Error on line $nr: $e"
    }
}