##amf3 and sol file(flash cookies) parsing libray.

###install
```bash
runhaskell Setup.hs configure --prefix=$HOME --user
runhaskell Setup.hs build
runhaskell Setup.hs install
```
###demo
see main.hs
###uninstall
unregister
```bash
ghc-pkg unregister amf3
```
remove the library folder by hand
```bash
rm -r ~/lib/amf3-0.1.0.0
```
