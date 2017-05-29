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
        var fin: InputStream?
        var fout: OutputStream?
        var pos: Int = 0
        var inMemory: Bool = false
    }
    
    static func ReadChar(_ f: inout File) -> Character {
        guard let r = f.fin else { return "\0" }
        if r.hasBytesAvailable {
            var buffer = [UInt8](repeating: 0, count: 1)
            if r.read(&buffer, maxLength: 1) == 1 {
                f.pos += 1
                return Character(Int(buffer[0]))
            }
        }
        return "\0"
    }
    
    static func WriteChar(_ f: File, ch: Int8) {
        guard let w = f.fout else { return }
        var lch = Int(ch)
        if ch < 0 { lch += 256 }
        var buffer = [UInt8](repeating: UInt8(lch), count: 1)
        if w.hasSpaceAvailable {
            w.write(&buffer, maxLength: 1)
        }
    }
    
    static func WriteString(_ f: File, s: String) {
        for ch in s.characters {
            Files.WriteChar(f, ch: Int8(ch.unicodeValue()))
        }
    }
    
    static func Eof(_ f: File) -> Bool {
        return f.fin == nil || !f.fin!.hasBytesAvailable
    }
    
    static func Open(_ name: String, mode: String) -> File? {
        if let stream = InputStream(fileAtPath: name), mode == "r" {
            stream.open()
            if stream.hasBytesAvailable {
                return File(name: name, fin: stream, fout: nil, pos: 0, inMemory: false)
            }
        } else if mode == "wm" {
            // special memory-based stream
            let stream = OutputStream.toMemory()
            stream.open()
            if stream.hasSpaceAvailable {
                return File(name: name, fin: nil, fout: stream, pos: 0, inMemory: true)
            }
        } else if let stream = OutputStream(toFileAtPath: name, append: false) {
            stream.open()
            if stream.hasSpaceAvailable {
                return File(name: name, fin: nil, fout: stream, pos: 0, inMemory: false)
            }
        }
        return nil
    }
    
    static func Tell(_ f: File) -> Int { return f.pos }
    
    static func DumpToC (_ f: File) -> Bool {
        if let os = f.fout, f.inMemory {
            if let buffer = os.property(forKey: Stream.PropertyKey.dataWrittenToMemoryStreamKey) as? Data {
                let fname = f.name.replacingOccurrences(of: ".lola", with: ".c")
                let fh = Open(fname, mode: "w")
                var bytes = [UInt8](repeating: 0, count: buffer.count)
                (buffer as NSData).getBytes(&bytes, length: buffer.count)
                
                try? buffer.write(to: URL(fileURLWithPath: f.name), options: [])  // also create .lola output file
                
                // Output the file header
                let cname = f.name.replacingOccurrences(of: ".lola", with: "")
                WriteString(fh!, s: "const unsigned char \(cname)[] = { ")
                
                for cnt in 0..<buffer.count {
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
    
    static func Close(_ f: File) {
        f.fin?.close()
        f.fout?.close()
    }
    
}
