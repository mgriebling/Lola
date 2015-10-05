//
//  Files.swift
//  Lola
//
//  Created by Mike Griebling on 3 Oct 2015.
//  Copyright © 2015 Solinst Canada. All rights reserved.
//

import Foundation



class Files {
    
    typealias File = NSInputStream    
    
    static func ReadChar(f: File?) -> Character {
        return "\0"
    }
    
    static func WriteChar(f: File?, ch: Character) {
        
    }
    
    static func WriteString(f: File?, s: String) {
        
    }
    
    static func Eof(f: File?) -> Bool {
        return true
    }
    
    static func Open(name: String, mode: String) -> File? {
        return nil
    }
    
    static func Tell(f: File?) -> Int { return 0 }
    
    static func Close(f: File?) {}
    
}