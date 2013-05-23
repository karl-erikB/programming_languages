package org.grash

import java.util.regex.Matcher

import static Tokens.*

class Interpreter {
    
    final symbols
    
    Interpreter(symbols) {
        this.symbols = symbols;
    }
    
    def run(command, args) {
        switch (command) {
        case ~/prm\((.+)\)/:
            return prm(Matcher.lastMatcher[0][1], args)
            break
        case ~/seq\((.+)\)/:
            return seq(Matcher.lastMatcher[0][1], args)
            break
        case ~/alt\((.+)\)/:
            return alt(Matcher.lastMatcher[0][1], args)
            break
        case ~/set\((.+)\)/:
            return set(Matcher.lastMatcher[0][1], args)
            break
        default:
            return cmd(command, args)
            break
        }
    }
    
    private def prm(command, args) {
        final nextArgs = args.clone()
        
        if (command =~ /^$ACTION/) {
            /* TODO */
        } else {
            final tokens = command.split()
            assert tokens.size() > 0

            final params = tokens.drop(1)
            params.each { param ->
                nextArgs << symbols.getSymbol(param)
            }

            run tokens[0], nextArgs
        }
    }
    
    private def seq(command, args) {
        
    }
    
    private def alt(command, args) {
        
    }
    
    private def set(command, args) {
        
    }
    
    private def cmd(command, args) {
        final rawCommand = [ command ]
        rawCommand.addAll(args)

        println "Executing $rawCommand"

        final proc = rawCommand.execute()
        proc.waitFor()

        println proc.in.text

        return proc.exitValue()
    }
    
}

