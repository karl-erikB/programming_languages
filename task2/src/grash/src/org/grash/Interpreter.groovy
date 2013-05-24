package org.grash

import java.util.regex.Matcher

import static Tokens.*

class Interpreter {
    
    final symbols
    
    Interpreter(symbols) {
        this.symbols = symbols;
    }
    
    /* The top level interpreter function.
     * command is the entire remaining string to be interpreted, and
     * args is the current list of bound arguments. */
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

        /* We have two cases: either prm() contains yet another action, in which case
         * we need to descend yet further with our interpreter.
         * Otherwise, the first argument is a command we need to call. */
        
        def nextCommand
        def nextParams
        if (command =~ /^$ACTION/) {
            final actions = []
            nextParams = extractActions(command, actions).split()

            if (actions.size() != 1) {
                throw new IllegalSyntaxException("Primitive does not contain exactly one action")
            }

            nextCommand = actions[0]
        } else {
            final tokens = command.split()

            if (tokens.size() < 1) {
                throw new IllegalSyntaxException("Empty primitive action")
            }

            nextCommand = tokens[0]
            nextParams = tokens.drop(1)
        }

        /* Look up all arguments and bind their values to all child actions. */
        nextParams.each { nextArgs << symbols.getSymbol(it) }
        run nextCommand, nextArgs
    }
    
    private def seq(command, args) {
        runUntil command, args, { it != 0 }
    }
    
    private def alt(command, args) {
        runUntil command, args, {it == 0 }
    }

    /* Extracts a list of actions from command, and executes
     * each in turn until shouldStop evaluates to true. The
     * exit code of the last action is returned. */
    private def runUntil(command, args, shouldStop) {
        final actions = []
        extractActions(command, actions)

        if (actions.size() == 0) {
            throw new IllegalSyntaxException("Empty action")
        }

        /* Loops until an execution returns != 0. */
        def exitCode = 0
        actions.find {
            exitCode = run(it, args)
            return shouldStop(exitCode)
        }
        
        return exitCode
    }
    
    /* For each token of a set parameter, binds that token to the bound symbol
     * and executes the given action until either all tokens have been processed
     * or an action returns != 0. As always, returns the last exit code. */
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
    
    /* Executes the command with all given arguments,
     * prints its output, and returns its exit code. */
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
     * list. The remaining string is returned. */
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

