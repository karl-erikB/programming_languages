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

            if (actions.size() != 1) {
                throw new IllegalSyntaxException("Primitive does not contain exactly one action")
            }

            params.each { param ->
                nextArgs << symbols.getSymbol(param)
            }

            run actions[0], nextArgs
        } else {
            final tokens = command.split()

            if (tokens.size() < 1) {
                throw new IllegalSyntaxException("Empty primitive action")
            }

            final params = tokens.drop(1)
            params.each { param ->
                nextArgs << symbols.getSymbol(param)
            }

            run tokens[0], nextArgs
        }
    }
    
    private def seq(command, args) {
        final actions = []
        extractActions(command, actions)

        if (actions.size() == 0) {
            throw new IllegalSyntaxException("Empty sequence")
        }

        /* Loops until an execution returns != 0. */
        def exitCode = 0
        actions.find {
            exitCode = run(it, args)
            return exitCode != 0
        }
        
        return exitCode
    }
    
    private def alt(command, args) {
        final actions = []
        extractActions(command, actions)

        if (actions.size() == 0) {
            throw new IllegalSyntaxException("Empty alternative")
        }

        def exitCode = 0
        actions.find {
            exitCode = run(it, args)
            return exitCode == 0
        }
        
        return exitCode
    }
    
    private def set(command, args) {
        final matcher = (command =~ /($SYM)$WHITE_<-$WHITE_($SYM)$WHITE_:$WHITE_($ACTION)/)

        if (!matcher) {
            throw new IllegalSyntaxException("Invalid syntax in set: $command")
        }

        final boundSym = matcher[0][1]
        final setSym = matcher [0][2]
        final c = matcher[0][3]

        final setVal = symbols.getSymbol setSym

        final actions = []
        extractActions(c, actions)

        if (actions.size() != 1) {
            throw new IllegalSyntaxException("Set does not contain exactly one action")
        }

        def exitCode = 0
        setVal.split().find {
            symbols.addDefinition boundSym, it

            exitCode = run(actions[0], args)
            return exitCode != 0
        }
        
        return exitCode
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

            if (!(c =~ /^$WHITE_$ACTION/)) {
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

            if (level != 0) {
                throw new IllegalSyntaxException("Found malformed action: $c")
            }

            actions << c[0..j - 1].trim()
            i += j
        }
    }

}

