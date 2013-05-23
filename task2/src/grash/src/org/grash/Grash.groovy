package org.grash

import java.util.regex.Matcher

final symbols = new Symbols()

args.each() { arg ->
    def pair = arg.tokenize ':'
    assert pair.size == 2
    symbols.addForwardDefinition pair[0], pair[1]
}

System.in.eachLine() { line, nr ->
    switch (line) {
    case ~/par[ ]+([a-zA-Z0-9]+)/:
        symbols.addDeclaration Matcher.lastMatcher[0][1]
        break
    case ~/defp.*/: println 'defp'; break
    case ~/deff.*/: println 'deff'; break
    case ~/def[ ]+([a-zA-Z0-9]+)[ ]+(.+)/:
        symbols.addDefinition Matcher.lastMatcher[0][1], Matcher.lastMatcher[0][2]
        break
    case ~/prm\(.*\)/: println('prm'); break
    case ~/seq\(.*\)/: println('seq'); break
    case ~/alt\(.*\)/: println('alt'); break
    case ~/set\(.*\)/: println('set'); break
    default: println "Illegal statement on line $nr: $line"; break
    }    
}