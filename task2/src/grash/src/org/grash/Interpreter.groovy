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
            final actions = []
            final params = extractActions(command, actions).split()
            assert actions.size() == 1

            params.each { param ->
                nextArgs << symbols.getSymbol(param)
            }

            run actions[0], nextArgs
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
    
    /* Parses all top-level actions in command and places them into the action
     * list. The remaining string is returned.
     */
    private def extractActions(command, actions) {
        def i = 0       /* The current index we are looking at. */
        while (i < command.size()) {
            final c = command[i..command.size() - 1]

            if (!(c =~ /^[ ]*$ACTION/)) {
                return c
            }

            def level = 1
            def j = c.indexOf('(') + 1
            while (level > 0 && j < c.size()) {
                switch (c[j]) {
                case '(': level++; break
                case ')': level--; break
                default: break
                }

                j++
            }

            actions << c[0..j - 1].trim()
            i += j
        }
    }

}

