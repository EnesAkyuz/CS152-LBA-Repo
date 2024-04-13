import openai
import os

openai.api_key = os.getenv('OPENAI_API_KEY')

def get_city_info(query):
    try:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are an assistant knowledgeable about various cities and the favorite study spots in these cities. These 7 cities are Buenos Aires, Berlin, London, San Francisco, Hyderabad and Seoul!"},
                {"role": "user", "content": query}
            ]
        )
        return response.choices[0].message['content'].strip()
    except Exception as e:
        return str(e)


if __name__ == "__main__":
    while True:
        user_query = input("What kind of place would you like to study today? (Type \"quit to quit\")")
        if user_query.lower() == 'quit':
            break
        print("GPT-4:", get_city_info(user_query))