# VIC 20 Dead Test cartridge

Code based on source code from Simon Rowe. Most of the testing routines are the same but I have added in a few extra tweaks.

- NTSC/PAL check
- Display chip information
- Change formatting of screen
- Loading of screen before any checks have been done
-

Orignal version can be found at https://eden.mose.org.uk/gitweb/?p=dead-test.git;a=summary



## Usage

This program will run from a cartridge at address $A000
When powering on the program will detect if you are using PAL or NTSC and set the appropriate video modes. 
Tests will then be run on the lower RAM, followed by video RAM.
  - Lower RAM $0000-$03FF
  - Video RAM $9400-$97FF

If there is a fault found with any of these tests the code will display the error and also show the chip number.

Following this the following RAM is tested:
  * BLK1,2,3 RAM from expansion cartridges
  * 1Kb Expansion $0400-$07FF - RAM1
  * 1Kb Expansion $0800-$0BFF - RAM2
  * 1Kb Expansion $0C00-$0FFF - RAM1
  * Main RAM address $1000-$13FF (1Kb)  - RAMa
  * Main RAM address $1400-$17FF (1Kb)  - RAMb
  * Main RAM address $1800-$1BFF (1Kb)  - RAMc
  * Main RAM address $1C00-$1FFF (1Kb)  - RAMd
  
  
On each 1Kb of RAM the upper and lower nibble of data is checked, followed by address and data lines. If the test fails on the nibble then data/address lines will not be tested and the code will then move onto the next block.

On each failure the status screen will also be updated with the chip number that is faulty. 

Following these tests the CRC value is calculated for each ROM (Basic, Char, Kernal) and if a matching version is found the status page will be updated. 

The code currently checks for 
* 901460-02
* 901460-03
* 901486-01
* 901486-02
* 901486-06
* 901486-07
* JiffyDOS PAL KERNAL
* JiffyDOS NTSC KERNAL

# Interpreting the output
When testing the main RAM, its broken down into 4 tests
* Upper nibble
* Lower nibble
* Address lines 
* Data lines

On the below screenshot the lower RAM test fails and no further tests are performed.

![Lower RAM test](https://github.com/StormTrooper/Vic20-dead-test/blob/master/images/fail-lower.png?raw=true)


On the below screenshot:
* Upper nibble from RAMa (1000-$13FF) passes
* Lower nibble from RAMa (1000-$13FF) fails with chip reference shown
* ROM versions are calculated with CRC and shown

![RAM Fail](https://github.com/StormTrooper/Vic20-dead-test/blob/master/images/fail2.png?raw=true)



On the below screenshot:
* Upper nibble from RAMa (1000-$13FF) passes
* Lower nibble from RAMa (1000-$13FF) fails with chip reference shown
* Upper nibble from RAMc (1800-$1BFF) fails with chip reference shown
* Lower nibble from RAMc (1800-$1BFF) fails with chip reference shown

![RAM Fail](https://github.com/StormTrooper/Vic20-dead-test/blob/master/images/fail1.png?raw=true)

On the below screenshot all tests pass.

![RAM Pass](https://github.com/StormTrooper/Vic20-dead-test/blob/master/images/pass.png?raw=true)


I've tested my changes on VICE and various VIC 20's and it seems to work fine.  However I am not a programmer so some of the changes I have made are bound to have a few bugs in. Also my coding is sketchy at best and not the most efficient so use at your own risk. 

Let me know if you find any bugs, errors or have any improvements to the code.
