from distutils.core import setup

setup(
    name='codescanner',
    version='1.0.0',
    scripts=['bin/codescanner.py','bin/fortranscanner.py','bin/ccppscanner.py','bin/scanner.py',], 
    license='MIT',
    include_package_data=True, 
    long_description=open('README.txt').read(),
    entry_points = {
        'console_scripts': ['codescanner=codescanner:main'],
    }
	
)
