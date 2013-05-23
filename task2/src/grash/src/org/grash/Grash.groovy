package org.grash

import java.util.regex.Matcher

final symbols = new Symbols()

args.each() { arg ->
    def pair = arg.tokenize ':'
    assert pair.size == 2
    symbols.addForwardDefinition pair[0], pair[1]
}

final rwhite = '[ ]+'
final rsym = '[a-zA-Z0-9]+'
final rregexp = '[^:]+'
final rval = '[^:]+'

System.in.eachLine() { line, nr ->
    try {
        switch (line) {
        case ~/par$rwhite($rsym)/:
            symbols.addDeclaration Matcher.lastMatcher[0][1]
            break
        case ~/defp$rwhite($rsym)$rwhite($rsym):($rregexp):($rval)/:
            final sym = Matcher.lastMatcher[0][1]
            final val = symbols.getSymbol Matcher.lastMatcher[0][2]
            final pattern = Matcher.lastMatcher[0][3]
            final replace = Matcher.lastMatcher[0][4]
            symbols.addDefinition sym, (val =~ pattern).replaceAll(replace)
            break
        case ~/deff.*/: println 'deff'; break
        case ~/def$rwhite($rsym)$rwhite($rval)/:
            symbols.addDefinition Matcher.lastMatcher[0][1],
                                  Matcher.lastMatcher[0][2]
            break
        case ~/prm\(.*\)/: println('prm'); break
        case ~/seq\(.*\)/: println('seq'); break
        case ~/alt\(.*\)/: println('alt'); break
        case ~/set\(.*\)/: println('set'); break
        default: println "Illegal statement on line $nr: $line"; break
        }
    } catch (Exception e) {
        println "Error on line $nr: $e"
    }
}