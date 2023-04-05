# Description

This folder contains the script used to produce the example Zarr arrays found
in the inst/extdata/zarr_examples folder.  These are created using the 
reference implementation of the Zarr specification found in 
[zarr-python](https://github.com/zarr-developers/zarr-python).

The files are used by the Rarr's unit tests to check reading consistency with
the reference implementation across a range of features.  The files aim to test
the following aspects of the specification:

* various data types e.g. different sized integers, floats, characters
* column-first and row-first layouts
* different compression tools
* special case fill values e.g. infinity

The naming of the generated files should be indicative of the feature under 
test.

# Generating the data

## Natively

If you have a python installation already available you can use either `pip`
or `conda` to install `zarr-python` as appropriate.

```
pip install zarr
```

or

```
conda install -c conda-forge zarr
```

You can then run the script using python:

```
python ./create_test_data.py
```

This will try to create files in the folder `/data`.  If thia location does not
exist you will either need to create it yourself or modify the relevant paths
in `create_test_data.py`.

## Using Docker

If you do not have a version of python and zarr readily available, you can
alternatively use the Dockerfile provided to generate the data.  The following
command will build a container called `zarr_tester`.

```
docker build -t zarr-tester .   
```

To generate the data you then use the command below.  This will produce the
example files in the folder `/tmp/zarr` on your local filesystem.  Modify that
path if it is not appropriate for your system.

```
mkdir /tmp/zarr_examples
docker run --rm --name zarr-tester --user ${UID}:${UID} \
  -v /tmp/zarr_examples:/data \
  zarr-tester
```
