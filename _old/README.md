# example_grammars

Example grammars for p1, p3, p4

## checkout

  * Checkout dev branches of p1 e3 and p4 into the same directory.

  * Checkout example_grammars (this repo) into the same directory.

The following should work:

```
git clone -b dev https://github.com/tomjridge/e3.git
git clone -b dev https://github.com/tomjridge/p1.git
git clone -b dev https://github.com/tomjridge/p4.git
git clone https://github.com/tomjridge/example_grammars.git
```


## build


Execute the following commands (taken from `local/test_release.sh`):

```
  (cd p1 && make && make install)
  (cd e3 && make && make install)
  export PATH=$PWD/p1/build:$PATH  # $PWD should be absolute path
  (cd p4 && make && make install)
  cd example_grammars && make
```
