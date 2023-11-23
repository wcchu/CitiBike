ENV_DIR=venv
rm -rf ${ENV_DIR}
virtualenv -p python3 ${ENV_DIR}
source ${ENV_DIR}/bin/activate
pip install --upgrade pip
pip install -r requirements.txt
