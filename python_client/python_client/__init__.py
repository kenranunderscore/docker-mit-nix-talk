__version__ = '0.1.0'

from loguru import logger
import requests

calculatorServiceUrl = "http://localhost:8081/expensiveProduct"

def make_payload(n1, n2):
    return {"number1": n1, "number2": n2}

def demo(n1, n2):
    logger.info(f'Requesting calculation of {n1} * {n2}')
    response = requests.post(calculatorServiceUrl, json=make_payload(n1, n2))
    logger.info("Result: " + response.text)

def main():
    demo(2, 183)
    demo(-5, 187)
    demo(17, -12)

if __name__ == "__main__":
    main()
