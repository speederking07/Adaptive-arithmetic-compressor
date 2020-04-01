/*
    Adaptive arithmetic compression with scaling

    Marek Bauer 2020
*/

use std::fs::File;
use std::path::Path;
use std::io::{Write, Read};
use std::env;

/**
    Struct representing probabilities of characters in file
*/
#[derive(Debug)]
struct Probabilities {
    sum: u64,
    pro: Vec<u64>,
    temp: Vec<u64>,
    cycle: u64,
}

impl Probabilities {
    const PRECISION: u64 = 1048576 * 1024; //Select the denominator of probabilities
    const CYCLE: u64 = 64; //Select length of cycle

    fn new() -> Self {
        let mut pro = vec![0; 257];
        for i in 0..257_u64 {
            pro[i as usize] = i;
        }
        Probabilities {
            sum: 256,
            temp: vec![0; 256],
            pro,
            cycle: 0,
        }
    }

    /**
    Add char to probability computations
    */
    fn add(&mut self, to_add: usize) {
        self.temp[to_add] += 1;
        self.cycle += 1;
        if self.cycle >= Self::CYCLE {
            self.cycle = 0;
            self.update_probabilities();
        }
    }

    /**
    Update probabilities
    */
    fn update_probabilities(&mut self) {
        let sum = self.temp.iter().fold(0_u64, |a, b| a + *b);
        let c = (Self::PRECISION - 256) as f64 / sum as f64;
        let mut t: Vec<u64> = vec![];
        //Compute probabilities to add up to PRECISION
        for v in self.temp.iter() {
            t.push((*v as f64 * c).floor() as u64 + 1);
        }
        let all = t.iter().fold(0_u64, |a, b| a + *b);
        let deficit = Self::PRECISION - all;
        //Rest add to first elements
        for i in 0_usize..deficit as usize {
            t[i] += 1;
        }
        let mut temp = 0;
        //Update pro array
        self.pro = vec![0; 257];
        for i in 1_usize..257 {
            temp += t[i - 1];
            self.pro[i] = temp;
        }
        assert_eq!(self.pro[256], Self::PRECISION);
        self.sum = Self::PRECISION;
    }
}

/**
    Struct representing coded sequence
*/
#[derive(Debug)]
struct Code {
    data: Vec<u8>,
    write_index: usize,
    read_index: usize,
    entropy: f32,
    chars: usize, //number of character encoded
}

impl Code {
    const BIN: [u8; 8] = [128, 64, 32, 16, 8, 4, 2, 1];

    fn new() -> Self {
        Self {
            data: Vec::new(),
            write_index: 0,
            read_index: 0,
            entropy: 0.0,
            chars: 0,
        }
    }

    /**
        Add bit to code
    */
    fn add_bit(&mut self, c: bool) {
        let sector = self.write_index % 8;
        if sector == 0 {
            self.data.push(0);
        }
        if c {
            let block = self.write_index / 8;
            self.data[block] |= Self::BIN[sector]
        }
        self.write_index += 1;
    }

    /**
        Read one bit of your code
    */
    fn get_bit(&self, i: usize) -> bool {
        return !self.data[i / 8] & Self::BIN[i % 8] == 0;
    }

    /**
        Read one bit and shift read_index
    */
    fn get_bit_and_shift(&mut self) -> bool {
        if self.read_index < (self.data.len() * 8) {
            self.read_index += 1;
            return self.get_bit((self.read_index - 1) as usize);
        }
        false
    }

    fn print_bar(p: u32){
        print!("|");
        for _i in 0..p{
            print!("█");
        }
        for _i in p..100{
            print!(" ");
        }
        println!("| {}%", p);
    }

    fn write_to_file<X>(&self, path: X) -> Result<(), String> where X: AsRef<Path> {
        let mut file;
        match File::create(path){
            Ok(f) => file = f,
            Err(_e) => return Err("Unable to open file".parse().unwrap())
        }
        let s0 = (self.chars % 256) as u8;
        let s1 = (self.chars / 256 % 256) as u8;
        let s2 = (self.chars / 65536 % 256) as u8;
        let s3 = (self.chars / 16777216 % 256) as u8;
        //Save data
        match file.write(self.data.as_ref()){
            Err(_e) => return Err("Unable to save file".parse().unwrap()),
            _ => {}
        }
        //Save number of encoded characters
        match file.write([s3, s2, s1, s0].as_ref()){
            Err(_e) => return Err("Unable to save file".parse().unwrap()),
            _ => {}
        }
        match file.sync_all(){
            Err(_e) => return Err("Unable to save file".parse().unwrap()),
            _ => {}
        }
        Ok(())
    }

    fn read_from_file<X>(path: X) -> Result<Self, String> where X: AsRef<Path> {
        let mut file;
        match File::open(path){
            Ok(f) => file = f,
            Err(_e) => return Err("Unable to open file".parse().unwrap())
        }
        let mut data = vec![];
        match file.read_to_end(data.as_mut()) {
            Err(_e) => return Err("Unable to read file".parse().unwrap()),
            _ => {}
        }
        let mut chars = 0;
        //Read number of encoded characters
        for i in 0_usize..=3 {
            let x = data.pop().expect("Wrong file");
            chars += x as usize * 256_u32.pow(i as u32) as usize;
        }
        Ok(Self {
            write_index: (data.len() * 8),
            data,
            chars,
            entropy: 0.0,
            read_index: 0,
        })
    }

    /**
        Returns one encoded char by value
    */
    fn get_code(low: u32, high: u32, val: u32, prob: &Probabilities) -> u8 {
        let range = high as u64 - low as u64 + 1;
        for i in 1 as u8..=255 {
            if (val as u64) <= (low as u64) + (range * prob.pro[i as usize] as u64) / prob.sum as u64 - 1 {
                return i - 1;
            }
        }
        255
    }

    fn decode(&mut self) -> Vec<u8> {
        println!("Decoding...");
        let mut prob = Probabilities::new();
        let mut res = Vec::new();
        let mut high = 0xFFFFFFFF_u32;
        let mut low = 0_u32;
        let mut value = 0_u32;
        let mut percent = 0;
        for _ in 0..32 {
            value <<= 1;
            if self.get_bit_and_shift() {
                value += 1;
            }
        }
        loop {
            let range = high as u64 - (low as u64) + 1;
            let c = Self::get_code(low, high, value, &prob);
            res.push(c);
            high = (low as u64 + (range * prob.pro[c as usize + 1] as u64) / prob.sum as u64 - 1) as u32;
            low = (low as u64 + (range * prob.pro[c as usize] as u64) / prob.sum as u64) as u32;
            if res.len()*100 / self.chars > percent{
                Self::print_bar(percent as u32);
                percent += 1;
            }
            if res.len() >= self.chars {
                break;
            }
            loop {
                if high < 0x80000000 {
                    //do nothing, bit is a zero
                } else if low >= 0x80000000 {
                    value -= 0x80000000;  //subtract one half from all three code values
                    low -= 0x80000000;
                    high -= 0x80000000;
                } else if low >= 0x40000000 && high < 0xC0000000 {
                    value -= 0x40000000;
                    low -= 0x40000000;
                    high -= 0x40000000;
                } else {
                    break;
                }
                low <<= 1;
                high <<= 1;
                high += 1;
                value <<= 1;
                if self.get_bit_and_shift() {
                    value += 1;
                }
            }
            prob.add(c as usize);
        }
        self.compute_entropy(&prob);
        res
    }

    fn encode<T>(data: T) -> Self
        where T: AsRef<[u8]>
    {
        println!("Encoding...");
        let mut prob = Probabilities::new();
        let d = data.as_ref();
        let mut code = Code::new();
        code.chars = d.len();
        let mut high = 0xFFFFFFFF_u32;
        let mut low = 0_u32;
        let mut pending_bits = 0_u32;
        let mut position = 0;
        let mut percent :u32 = 0;
        for c in d {
            position += 1;
            if position*100 / d.len() > percent as usize {
                Self::print_bar(percent);
                percent += 1;
            }
            let range = high as u64 - low as u64 + 1;
            high = (low as u64 + (range * prob.pro[*c as usize + 1] as u64) / prob.sum as u64 - 1) as u32;
            low = (low as u64 + (range * prob.pro[*c as usize] as u64) / prob.sum as u64) as u32;
            loop {
                if high < 0x80000000_u32 {
                    code.add_bit(false);
                    for _ in 0..pending_bits {
                        code.add_bit(true);
                    }
                    pending_bits = 0;
                    low <<= 1;
                    high <<= 1;
                    high |= 1;
                } else if low >= 0x80000000_u32 {
                    code.add_bit(true);
                    for _ in 0..pending_bits {
                        code.add_bit(false);
                    }
                    pending_bits = 0;
                    low <<= 1;
                    high <<= 1;
                    high |= 1;
                } else if low >= 0x40000000_u32 && high < 0xC0000000_u32 {
                    pending_bits += 1;
                    low <<= 1;
                    low &= 0x7FFFFFFF;
                    high <<= 1;
                    high |= 0x80000001;
                } else {
                    break;
                }
            }
            prob.add(*c as usize);
        }
        code.add_bit(true);
        code.compute_entropy(&prob);
        code
    }

    fn print_compression_statistics(&self){
        println!("Size before compression: {}B", self.chars);
        println!("Size after compression: {}B", self.data.len());
        println!("Compression ratio: {}%", self.data.len() as f32 * 100.0 / self.chars as f32);
        println!("Entropy: {}", self.entropy);
    }

    fn compute_entropy(&mut self, p :&Probabilities){
        let sum = p.temp.iter().fold(0, |a, b| a+*b);
        self.entropy = p.temp.iter().fold(0.0, |acc, x| if *x > 0{
            acc - (*x as f32/ sum as f32) * ((*x) as f32 / sum as f32).log2()
        }  else{
            acc
        });
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        4 => {
            match args[1].as_str() {
                "--encode" => {
                    let mut file;
                    match File::open(args[2].clone()) {
                        Ok(f) => file = f,
                        Err(_error) => {
                            println!("Unable to open file {}", args[2]);
                            return;
                        }
                    }
                    let mut data = vec![];
                    match file.read_to_end(data.as_mut()) {
                        Err(_e) => {
                            println!("Unable to read file {}", args[2]);
                            return;
                        }
                        Ok(_) => {},
                    }
                    let code = Code::encode(data);
                    match code.write_to_file(args[3].clone()) {
                        Err(_e) => {
                            println!("Unable to write to file {}", args[3]);
                            return;
                        }
                        Ok(()) => {}
                    }
                    code.print_compression_statistics()
                }
                "--decode" => {
                    let mut code = Code::read_from_file(args[2].clone()).expect("Unable to open file");
                    let data = code.decode();
                    code.print_compression_statistics();
                    let mut file;
                    match File::create(args[3].clone()) {
                        Ok(f) => file = f,
                        Err(_error) => {
                            println!("Unable to create file {}", args[2]);
                            return;
                        }
                    }
                    match file.write_all(data.as_ref()) {
                        Err(_e) => {
                            println!("Unable to write file {}", args[2]);
                            return;
                        }
                        Ok(_) => {},
                    }
                    match file.sync_all() {
                        Err(_e) => {
                            println!("Unable to write file {}", args[2]);
                            return;
                        }
                        Ok(_) => {},
                    }
                }
                _ => println!("Wrong arguments please try {} <--encode | --decode> <file_from> <file_to>", args[0])
            }
        }
        _ => println!("Wrong arguments please try {} <--encode | --decode> <file_from> <file_to>", args[0])
    }
}