# Ada-DES
This is a Data Encryption Standard implemented in Ada. The main purpose of this project was to learn more not only about this language, but also about this block cipher. 

The DES encrypts blocks of 64-bit with a encryption key of 64-bit. This implementation supports ECB and CBC modes of operation.

### How to use

The following command will create the executable in the root folder:
```
gprbuild -Pdes
```

Usage:
```
ades -e -f <file> --key=<key>
 -e          Encrypt mode
 -d          Decrypt mode
 -f <file>   Input Filepath
 -o <file>   Output Filepath
 --Key       64bit Hexadecimal key
 -h --help
```
Example:
```
ades -e -f plain.txt --key=2F423F4528482B4D
ades -d -f ciphertext -o output.txt --key=2F423F4528482B4D
```

#### The algorithm was implemented following these documents

https://academic.csuohio.edu/yuc/security/Chapter_06_Data_Encription_Standard.pdf

http://page.math.tu-berlin.de/~kant/teaching/hess/krypto-ws2006/des.htm

https://csrc.nist.gov/csrc/media/publications/fips/46/3/archive/1999-10-25/documents/fips46-3.pdf
