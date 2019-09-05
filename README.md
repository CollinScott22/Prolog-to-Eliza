# Prolog-to-Eliza


This file is my interpretation of the Eliza program written in Prolog. 

It works by following a set procedure:

1. It asks for an input.
2. Once that input is submitted by pressing 'Enter' on the keyboard, it scans the text character by character until it finds a blank space.
3. It defines that series of character as a word
4. Eliza checks that word against a collection of keywords and synonyms
5a. If it finds a match, it prints a given response and awaits further input.

5b. If no match is found with that first word, it deletes the word from the given phrase and continues to the next character that isn't punctutation or a blank space. Repeats steps 2-4

6. If no matches are found, it gives a generic response and awaits further input
