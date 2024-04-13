# CS152-LBA-Repo
CS152 LBA repository for 2024 Spring. Managed by: Davi Coscarelli, Rishabh Java, Enes Akyuz and Muhammad Saleh


python3 -m venv pyswip_env
source pyswip_env/bin/activate

Get the version 8.4.2 of SWI-Prolog from http://www.swi-prolog.org/Download.html and install it.

pip install git+https://github.com/yuce/pyswip@master#egg=pyswip
export DYLD_FALLBACK_LIBRARY_PATH=/Applications/SWI-Prolog.app/Contents/Frameworks
export PATH=$PATH:/Applications/SWI-Prolog.app/Contents/MacOS

pip install -r requirements.txt
python main.py