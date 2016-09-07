import gtk # gimp tool kit bindings
import gtk.glade # glade bindings
import urllib2
import gobject	
from avc import * # AVC

test_GLADE_XML = '/home/andante/Bitcoin/UI/wallet2_.glade'
GLADE_XML = 'gtk_spinbutton.glade' # GUI glade descriptor

def request(address):
	http = urllib3.PoolManager()
	context = ssl.SSLContext(ssl.PROTOCOL_TLSv1)
	r = http.request('GET',address)
	return r

def addressRequest(address):
	link = 'https://blockchain.info/ru/rawaddr/'
	text = request(link + address)
	print (text)

class Example(AVC):
	def __init__(self):
		self.glade = gtk.glade.XML(test_GLADE_XML)
		self.glade.signal_autoconnect(self)
		self.Adr = "test"
	
	def on_button7_clicked(self, window):
		addressRequest(self.Adr)

	def on_destroy(self,window):
		gtk.main_quit()

example = Example() 
example.avc_init() 
gtk.main() 


	
