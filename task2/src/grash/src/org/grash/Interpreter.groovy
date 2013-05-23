package org.grash

import java.util.regex.Matcher

import static Tokens.*

class Interpreter {
    
    final symbols
    
    Interpreter(def symbols) {
        this.symbols = symbols;
    }
    
    def run(def command) {
        switch (line) {
        case ~/prm\(.+)/:
            return prm(Matcher.lastMatcher[0][1])
            break
        case ~/seq\(.+)/:
            return seq(Matcher.lastMatcher[0][1])
            break
        case ~/alt\(.+)/:
            return alt(Matcher.lastMatcher[0][1])
            break
        case ~/set\(.+)/:
            return set(Matcher.lastMatcher[0][1])
            break
        default:
            return cmd(command)
            break
        }
    }
    
    private def prm(def command) {
        
    }
    
    private def seq(def command) {
        
    }
    
    private def alt(def command) {
        
    }
    
    private def set(def command) {
        
    }
    
    private def cmd(def command) {
        
    }
    
}

