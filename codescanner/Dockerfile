# use python2 interpreter here. With some minor changes,
# we can use python3.
FROM python:3
COPY . /bin

# import needed python source files
ADD bin/scanner.py /
ADD bin/ccppscanner.py /
ADD bin/fortranscanner.py /
ADD bin/codescanner.py /

# install some command-line tools
RUN apt-get update
RUN apt-get install cloc -y 
RUN apt-get install cppcheck -y
RUN apt-get install flawfinder -y
RUN apt-get install -y shellcheck

# install necessary packages
RUN pip install pyparsing 

# these are the possible command-line inputs:
# -h','--help
# -d','--directory
# -v','--version
# -o','--output-directory
# -i','--ignore
ENTRYPOINT [ "python", "codescanner.py" ]
