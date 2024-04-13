import openai
import os
import telebot

# keys
BOT_TOKEN = os.getenv('T_TOKEN')
bot = telebot.TeleBot(BOT_TOKEN)
openai.api_key = os.getenv('OPENAI_API_KEY')

# Questions and options for the menus
questions_and_options = {
    "Select your city:": ["Seoul", "London", "Berlin", "Buenos Aires", "Hyderabad", "San Francisco", "Taipei"],
    "Do you prefer a quiet or lively atmosphere?": ["Quiet", "Lively"],
    "Do you require internet access?": ["Yes", "No"],
    "Do you need access to power outlets?": ["Yes", "No"],
    "Would you like to have the option to purchase food at the study spot? ": ["Yes", "No"],
    "What is your budget for spending at a study spot?": ["Free", "<5$", "5-10$", ">10$"],
    "How many star ratings should the study spot have? ": ["Any", "1", "2", "3", "4", "5"],
    "How long do you plan to study?": ["<2 hrs", "2-4 hrs", "4+ hrs", "Not sure"],
    "What are the modes of transport you prefer to use? ": ["Bus", "Train", "Bicycle", "Car", "No preference"],
    "How much time do you have available for commuting?": ["<5 mins", "5-15 mins", "15-30 mins", ">30"],
    "Is accessibility important to you? ": ["Yes", "No"]
}

# Dictionary to hold user session data
user_sessions = {}


def start_conversation(message):
    user_id = message.chat.id
    # Initialize user session
    user_sessions[user_id] = {
        'answers': {},
        'questions': list(questions_and_options.keys()),
        'current_question_index': 0
    }
    bot.send_message(user_id, "Hello Minervan! Let us start by learning about your address so that I can give you closer places:")


def ask_next_question(message, user_id):
    session = user_sessions[user_id]
    questions = session['questions']
    current_index = session['current_question_index']

    if current_index < len(questions):
        question = questions[current_index]
        options = questions_and_options[question]
        markup = telebot.types.InlineKeyboardMarkup(row_width=2)
        # Create a button for each option
        buttons = [telebot.types.InlineKeyboardButton(option, callback_data=option) for option in options]
        markup.add(*buttons)
        bot.send_message(user_id, question, reply_markup=markup)
    else:
        display_summary(message, user_id)


def handle_callback(call):
    user_id = call.message.chat.id
    session = user_sessions[user_id]
    question = session['questions'][session['current_question_index']]

    # Record the user's answer
    session['answers'][question] = call.data
    session['current_question_index'] += 1

    # Acknowledge the callback
    bot.answer_callback_query(call.id, "You selected: " + call.data)

    # Ask the next question or end
    ask_next_question(call.message, user_id)


def display_summary(message, user_id):
    session = user_sessions[user_id]
    response_text = "Here's the address you provided and your responses:\n\n"
    response_text += f"Address: {session['answers'].get('Address', 'Not provided')}\n\n"
    for question, answer in session['answers'].items():
        if question != 'Address':
            response_text += f"{question} \n {answer}\n\n"
    bot.send_message(user_id, response_text)
    # Optionally clear the session data if no longer needed
    del user_sessions[user_id]


@bot.message_handler(commands=['start'])
def send_welcome(message):
    start_conversation(message)


@bot.message_handler(
    func=lambda message: message.chat.id in user_sessions and 'Address' not in user_sessions[message.chat.id][
        'answers'])
def handle_address(message):
    user_id = message.chat.id
    user_sessions[user_id]['answers']['Address'] = message.text
    bot.send_message(user_id, "Thank you for your address. Now let's move on to some questions.")
    ask_next_question(message, user_id)


@bot.callback_query_handler(func=lambda call: True)
def callback_query(call):
    handle_callback(call)

@bot.message_handler(commands=['start', 'restart'])
def handle_start_or_restart(message):
    # Whether it's a start or a restart, treat it the same
    start_conversation(message)

def get_city_info(query):
    try:
        response = openai.ChatCompletion.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are an assistant knowledgeable about various cities and the favorite study spots in these cities. These 7 cities are Buenos Aires, Berlin, London, San Francisco, Hyderabad, Seoul and Taipei!"},
                {"role": "user", "content": query}
            ]
        )
        return response.choices[0].message['content'].strip()
    except Exception as e:
        return str(e)

if __name__ == "__main__":
    bot.polling()



