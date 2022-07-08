.PHONY : all clean distclean

all:
	/home/students/inf/PUBLIC/MRJP/bin/bnfc Gramma.cf
	happy -gca ParGramma.y
	alex -g LexGramma.x
	ghc --make interpreter.hs -o interpreter

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsGramma.hs AbsGramma.hs.bak ComposOp.hs ComposOp.hs.bak DocGramma.txt DocGramma.txt.bak ErrM.hs ErrM.hs.bak LayoutGramma.hs LayoutGramma.hs.bak LexGramma.x LexGramma.x.bak ParGramma.y ParGramma.y.bak PrintGramma.hs PrintGramma.hs.bak SkelGramma.hs SkelGramma.hs.bak TestGramma.hs TestGramma.hs.bak XMLGramma.hs XMLGramma.hs.bak ASTGramma.agda ASTGramma.agda.bak ParserGramma.agda ParserGramma.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak Gramma.dtd Gramma.dtd.bak TestGramma LexGramma.hs ParGramma.hs ParGramma.info ParDataGramma.hs Makefile


