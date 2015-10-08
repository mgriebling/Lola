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
        var name: String = ""
        var fin: NSInputStream?
        var fout: NSOutputStream?
        var pos: Int = 0
        var inMemory: Bool = false
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
        if w.hasSpaceAvailable {
            w.write(&buffer, maxLength: 1)
        }
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
                return File(name: name, fin: stream, fout: nil, pos: 0, inMemory: false)
            }
        } else if mode == "wm" {
            // special memory-based stream
            let stream = NSOutputStream.outputStreamToMemory()
            stream.open()
            if stream.hasSpaceAvailable {
                return File(name: name, fin: nil, fout: stream, pos: 0, inMemory: true)
            }
        } else if let stream = NSOutputStream(toFileAtPath: name, append: false) {
            stream.open()
            if stream.hasSpaceAvailable {
                return File(name: name, fin: nil, fout: stream, pos: 0, inMemory: false)
            }
        }
        return nil
    }
    
    static func Tell(f: File) -> Int { return f.pos }
    
    static func DumpToC (f: File) -> Bool {
        if let os = f.fout where f.inMemory {
            if let buffer = os.propertyForKey(NSStreamDataWrittenToMemoryStreamKey) as? NSData {
                let fname = f.name.stringByReplacingOccurrencesOfString(".lola", withString: ".c")
                let fh = Open(fname, mode: "w")
                var bytes = [UInt8](count: buffer.length, repeatedValue: 0)
                buffer.getBytes(&bytes, length: buffer.length)
                
                // Output the file header
                let cname = f.name.stringByReplacingOccurrencesOfString(".lola", withString: "")
                WriteString(fh!, s: "const unsigned char \(cname)[] = { ")
                
                for cnt in 0..<buffer.length {
                    let s = String(format: "0x%02X, ", bytes[cnt])
                    if cnt % 16 == 0 {
                        let addr = String(format: "0x%04X", cnt)
                        WriteString(fh!, s: "\n\t/* \(addr) */ ")
                    }
                    WriteString(fh!, s: s)
                }
                
                WriteString(fh!, s: "\n} \n")
                Close(fh!)
            }
        }
        return false
    }
    
    static func Close(f: File) {
        f.fin?.close()
        f.fout?.close()
    }
    
}