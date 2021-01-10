# codescanner

This is a Python 3 command-line program to scan scientific codes for security flaws, best practices.
     
## Installation

Installation occurs in a UNIX or Linux command-line environment (e.g. Mac OSX works too).
Requires docker (https://www.docker.com/) to be installed onto your system.
 
       $ git clone https://github.com/gerasimosmichalitsianos/codescanner
       $ cd codescanner/codescanner
       $ docker build -t codescanner .
       
## Usage

Once you build the docker image from the installation above, you just specify the
input directory of your source codes to be scanned, and run the docker container:
       
       $ DIR=/home/gmichali/CCAP_MiRS_v1-0_20201228_FilesToReview
       $ docker run -v $DIR:$DIR codescanner --directory $DIR
       
Alternatively, after checking out the code, you could simply do:
 
       $ DIR=/home/gmichali/CCAP_MiRS_v1-0_20201228_FilesToReview
       $ git clone https://github.com/gerasimosmichalitsianos/codescanner
       $ cd codescanner/codescanner
       $ python3 codescanner.py --directory $DIR

## @author: 
       Gerasimos Michalitsianos
       gerasimosmichalitsianos@gmail.com
       January 10th, 2021
