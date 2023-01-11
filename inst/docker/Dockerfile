FROM python:3.10
LABEL Maintainer=mike.smith@embl.de
RUN pip install zarr
RUN mkdir /data/
WORKDIR /usr/app/src
COPY create_test_data.py ./
CMD python ./create_test_data.py
