# semester-project
-----
Use this command for fast download

```
git clone --depth=1 https://github.com/Artemonchik/haskellMP3Tags/
```

This project includes fltkhs library:

```
stack build --flag fltkhs:bundled
stack run ghc
```
If you use Windows don't forget to read this guide
https://hackage.haskell.org/package/fltkhs-0.8.0.3/docs/Graphics-UI-FLTK-LowLevel-FLTKHS.html#g:13

**Description**
-----
This is mp3 tag editor. You can read and change 7 text tag types. You need to add absolute path to the field "DO Specify
 the path to the folder". And then press "Open folder button". Further it will be clear what to do  
