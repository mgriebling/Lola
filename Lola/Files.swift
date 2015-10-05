//
//  Files.swift
//  Lola
//
//  Created by Mike Griebling on 3 Oct 2015.
//  Copyright © 2015 Solinst Canada. All rights reserved.
//

import Foundation



class Files {
    
    struct File {
        var fin: NSInputStream?
        var fout: NSOutputStream?
        var pos: Int = 0
    }
    
    static func ReadChar(var f: File) -> Character {
        guard let r = f.fin else { return "\0" }
        if r.hasBytesAvailable {
            var buffer = [UInt8](count: 1, repeatedValue: 0)
            if r.read(&buffer, maxLength: 1) == 1 {
                f.pos++
                return Character(Int(buffer[0]))
            }
        }
        return "\0"
    }
    
    static func WriteChar(f: File, ch: Int8) {
        guard let w = f.fout else { return }
        var lch = Int(ch)
        if ch < 0 { lch += 256 }
        var buffer = [UInt8](count: 1, repeatedValue:UInt8(lch))
        w.write(&buffer, maxLength: 1)
    }
    
    static func WriteString(f: File, s: String) {
        for ch in s.characters {
            Files.WriteChar(f, ch: Int8(ch.unicodeValue()))
        }
    }
    
    static func Eof(f: File) -> Bool {
        return f.fin == nil || !f.fin!.hasBytesAvailable
    }
    
    static func Open(name: String, mode: String) -> File? {
        if let stream = NSInputStream(fileAtPath: name) where mode == "r" {
            stream.open()
            if stream.hasBytesAvailable {
                return File(fin: stream, fout: nil, pos: 0)
            }
        } else if let stream = NSOutputStream(toFileAtPath: name, append: false) {
            stream.open()
            return File(fin: nil, fout: stream, pos: 0)
        }
        return nil
    }
    
    static func Tell(f: File) -> Int { return f.pos }
    
    static func Close(f: File) {
        f.fin?.close()
        f.fout?.close()
    }
    
}