from __future__ import print_function
import json
import urllib2


# --------------- Helpers that build all of the responses ----------------------

def build_speechlet_response(output, should_end_session):
	return {
		'outputSpeech': {
			'type': 'PlainText',
			'text': output
		},
		'shouldEndSession': should_end_session
	}


def build_response(session_attributes, speechlet_response):
	return {
		'version': '1.0',
		'sessionAttributes': session_attributes,
		'response': speechlet_response
	}


# --------------- Functions that control the skill's behavior ------------------

def get_welcome_response():
	session_attributes = {}
	card_title = "Welcome"
	speech_output = "Hello World "
	reprompt_text = None
	should_end_session = True
	return build_response(session_attributes, build_speechlet_response(speech_output, should_end_session))


def handle_session_end_request():
	card_title = "Session Ended"
	speech_output = "Goodbye "
	should_end_session = True
	return build_response({}, build_speechlet_response(speech_output, should_end_session))


def hello_name(intent, session):
	session_attributes = {}
	itema = intent['slots']['itema']['value']
	itemb = intent['slots']['itemb']['value']
	itemc = intent['slots']['itemc']['value']
	itemd = intent['slots']['itemd']['value']
	iteme = intent['slots']['iteme']['value']
	itemf = intent['slots']['itemf']['value']


	reprompt_text = None
	speech_output = 'Hello {} Nice to Meet You'.format(itema)
	should_end_session = True
	return build_response(session_attributes, build_speechlet_response(speech_output, should_end_session))


def modify_state( port, state, token):
    url = 'http://localhost:8000' % (port, state, token)
    req = urllib2.Request(url,'')
    response = urllib2.urlopen(req)

def lambda_handler(event, context):
    modify_state('GroveRelayD0', <STATE:0:1>, '<APIKEY')
    # TODO implement
    return {
        'version': '1.0',
        'sessionAttributes': {},
        'response': {
            'outputSpeech': {
                'type': 'PlainText',
                'text': '<whatever witty remark alexa should say>'
            },
            'card': {
                'type': 'Simple',
                'title': "SessionSpeechlet - foo",
                'content': "SessionSpeechlet - bar"
            },
            'reprompt': {
                'outputSpeech': {
                    'type': 'PlainText',
                    'text': 'I know right'
                }
            },
            'shouldEndSession': True
        }
    }


# --------------- Specific Events ------------------

def on_intent(intent_request, session):
	print("on_intent requestId=" + intent_request['requestId'] + ", sessionId=" + session['sessionId'])
	intent = intent_request['intent']
	intent_name = intent_request['intent']['name']
	if intent_name == "HelloName":
		return hello_name(intent, session)
	elif intent_name == "AMAZON.HelpIntent":
		return get_welcome_response()
	elif intent_name == "AMAZON.CancelIntent" or intent_name == "AMAZON.StopIntent":
		return handle_session_end_request()
	else:
		raise ValueError("Invalid intent")

# --------------- Generic Events ------------------

def on_session_started(session_started_request, session):
	print("on_session_started requestId=" + session_started_request['requestId']+ ", sessionId=" + session['sessionId'])

def on_launch(launch_request, session):
	print("on_launch requestId=" + launch_request['requestId'] + ", sessionId=" + session['sessionId'])
	return get_welcome_response()

def on_session_ended(session_ended_request, session):
	print("on_session_ended requestId=" + session_ended_request['requestId'] + ", sessionId=" + session['sessionId'])


# --------------- Main handler ------------------

def lambda_handler(event, context):
	print("event.session.application.applicationId=" + event['session']['application']['applicationId'])
	if event['session']['new']:
		on_session_started({'requestId': event['request']['requestId']}, event['session'])
	if event['request']['type'] == "LaunchRequest":
		return on_launch(event['request'], event['session'])
	elif event['request']['type'] == "IntentRequest":
		return on_intent(event['request'], event['session'])
	elif event['request']['type'] == "SessionEndedRequest":
		return on_session_ended(event['request'], event['session'])
