FROM python:3.10
LABEL Maintainer=mike.smith@embl.de
RUN pip install zarr zarrita
RUN mkdir /data/
WORKDIR /usr/app/src
COPY create_test_data.py ./
COPY create_test_data_v3.py ./
ENV ZARR_V3_EXPERIMENTAL_API=1
CMD python ./create_test_data.py && python ./create_test_data_v3.py
