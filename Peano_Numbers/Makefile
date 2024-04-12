CXX = g++
CXXFLAGS = -std=c++11

SRCS   := test_add.cpp


OBJS   := ${SRCS:cpp=o}
PROGS  := ${SRCS:.cpp=}

.PHONY: all
all: ${PROGS}

${PROGS} : % : %.o Makefile
	${CXX} $< -o $@ ${LIBS}

clean:
	rm -f ${PROGS} ${OBJS}

%.o: %.cpp Makefile
	${CXX} ${CXXFLAGS} -c $<