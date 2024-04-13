import openai
import os
import telebot

# keys
BOT_TOKEN = os.getenv('T_TOKEN')
bot = telebot.TeleBot(BOT_TOKEN)
openai.api_key = os.getenv('OPENAI_API_KEY')

@bot.message_handler(commands=['start', 'help'])
def send_welcome(message):
    bot.reply_to(message, "Hey Minervan! I am your study assistant. I recommend places in your rotation cities, please start by telling me your rotation city!")


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

@bot.message_handler(func=lambda message: True)
def handle_message(message):
    response_text = get_city_info(message.text)
    bot.reply_to(message, response_text)

if __name__ == "__main__":
    bot.polling()
