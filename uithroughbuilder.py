import json
import gi
import urllib2
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
from gi.repository import Gdk
from gi.repository import GdkPixbuf
Gdk.threads_init()


author = "1AJbsFZ64EpEfS5UAjAfcUG8pH8Jn3rn1F"


def read_configuration(address=None):
	f = open('config', 'r')
	conf = json.loads(f.read())
	if (conf != None or conf !=''):
		author = (conf['config'])['user_address']
	print "Own address was changes due to the configuration information"
	print "User address is now " + author

def request (address):
	req = urllib2.Request(url=address)
	f = urllib2.urlopen(req)
	r = f.read()
	parsed = json.loads(r)
	return parsed

class ToSend():

	def __init__(self, value, address, proof):
		self.value = value
		self.address = address
		self.proof = proof



class TreeViewFilterWindow(Gtk.Window):


	def requestTransactionInformation(self):

		tree_selection = self.treeview.get_selection()
		(model, pathlist) = tree_selection.get_selected_rows()
   		for path in pathlist :
        		tree_iter = model.get_iter(path)
        		value = model.get_value(tree_iter,1)

		link = "https://blockchain.info/rawtx/" + value
		result = request(link)
		lst = list()
		parsedTransactions = result["out"] 
		for i in range (len(parsedTransactions)):
			lst.append(( parsedTransactions[i]["addr"], parsedTransactions[i]["value"]))
		return lst

	def AddDeleteNewElement(self, widget=None, path = None, column = None):
			return newReceiveForm(widget,path,column)

	def newReceiveForm(self, widget = None, path = None, column = None):

		receiveForm = Gtk.Window()
		receiveForm.set_title("Send to..")
		self.receiveGrid = Gtk.Grid()
		self.receiveGrid.set_column_homogeneous(False)
		self.receiveGrid.set_row_homogeneous(True);
		self.receiveLabels = list()
		self.receiveEntries = list()
		for i,el in enumerate(["Value", "Address", "Proof"]):
			label = Gtk.Label(el, xalign = 0)
			self.receiveLabels.append(label)
			self.receiveEntries.append(Gtk.Entry())

		self.receiveGrid.attach(self.receiveLabels[0],0,0,1,1)
		self.receiveGrid.attach_next_to(self.receiveEntries[0], self.receiveLabels[0], Gtk.PositionType.RIGHT, 1,1)
		self.receiveGrid.attach_next_to(self.receiveLabels[1],self.receiveLabels[0],Gtk.PositionType.BOTTOM,1,1)
		self.receiveGrid.attach_next_to(self.receiveEntries[1],self.receiveEntries[0],Gtk.PositionType.BOTTOM,1,1)
		self.receiveGrid.attach_next_to(self.receiveLabels[2],self.receiveLabels[1],Gtk.PositionType.BOTTOM,1,1)
		
		self.comboProofModule = Gtk.ComboBoxText()
		self.comboProofModule.append_text("A")
		self.comboProofModule.append_text("B")
		self.comboProofModule.append_text("C")

		self.comboProofModule.set_entry_text_column(1)
		self.receiveGrid.attach_next_to(self.comboProofModule, self.receiveEntries[1], Gtk.PositionType.BOTTOM, 1,1)

		self.buttonAddR = Gtk.Button("Add")
		self.buttonCancelR = Gtk.Button("Cancel")

		def dest(self, widget=None):
			receiveForm.destroy()
			return False

		def save(self, widget = None):
			a = ToSend(self.receiveLabels[0].get_text(), self.receiveLabels[1].get_text(), self.comboProofModule.get_active_text())
			return a

		self.buttonCancelR.connect("clicked",dest)	
		self.buttonAddR.connect("clicked",save)

		self.receiveGrid.attach_next_to(self.buttonAddR,self.receiveLabels[2],Gtk.PositionType.BOTTOM,1,1)	
		self.receiveGrid.attach_next_to(self.buttonCancelR, self.comboProofModule,Gtk.PositionType.BOTTOM, 1,1)	
		receiveForm.add(self.receiveGrid)
		receiveForm.set_modal( True )
		receiveForm.set_transient_for( self )
		receiveForm.show_all()

	def popup(self, widget, path, column):

		popup = Gtk.Window()
        	popup.set_title( "Properties" )
		lst = self.requestTransactionInformation()

		self.popup_grid = Gtk.Grid()
		self.popup_grid.set_column_homogeneous(False)
		self.popup_grid.set_row_homogeneous(False)	

		self.poplabels = list()
		for i in range (len(lst)):
			label = Gtk.Label("Sent to:     " + lst[i][0], xalign=0)
			self.poplabels.append(label)
			label = Gtk.Label("Amount:    " +str(lst[i][1]), xalign=0)
			self.poplabels.append(label)
		
		def dest(self, widget=None):
			popup.destroy()
			return False	
		
		popup.connect("destroy", dest)

		self.popup_grid.attach(self.poplabels[0],0,0,2,2)
		for i in range (1,len(self.poplabels)):
			self.popup_grid.attach_next_to(self.poplabels[i],self.poplabels[i-1],Gtk.PositionType.BOTTOM,2,2)
		
		self.buttonAdd = Gtk.Button("Add")
		self.buttonCancel = Gtk.Button("Close")
		self.buttonCancel.connect("clicked",dest)

		self.popup_grid.attach_next_to(self.buttonAdd, self.poplabels[len(self.poplabels)-1],Gtk.PositionType.BOTTOM, 1,1)
		self.popup_grid.attach_next_to(self.buttonCancel, self.buttonAdd, Gtk.PositionType.RIGHT, 1,1)
        	popup.add(self.popup_grid);

		popup.set_modal( True )
		popup.set_transient_for( self )

		popup.show_all()
		
	def __init__(self):

		read_configuration()

		Gtk.Window.__init__(self, title="Wallet")
		self.set_border_width(10)

		self.main_layer = Gtk.Notebook()
		self.main_layer.set_size_request(700, 400)
		self.add(self.main_layer)
	

		#Setting up the self.grid in which the elements are to be positionned
		self.grid = Gtk.Grid()
		self.grid.set_column_homogeneous(False)
		self.grid.set_row_homogeneous(False)
		
		#Creating the ListStore model
		self.software_liststore = Gtk.ListStore(int, str)
		self.current_filter_language = None

		#Creating the filter, feeding it with the liststore model
		self.language_filter = self.software_liststore.filter_new()
		#setting the filter function, note that we're not using the
		self.language_filter.set_visible_func(self.language_filter_func)

		#creating the treeview, making it use the filter as a model, and adding the columns
		self.treeview = Gtk.TreeView.new_with_model(self.language_filter)
		self.treeview.connect('row-activated',self.popup)


		#self.treeview.set_height_request(100)
		for i, column_title in enumerate(["ID", "Hash"]):
		    renderer = Gtk.CellRendererText()
		    column = Gtk.TreeViewColumn(column_title, renderer, text=i)
		    self.treeview.append_column(column)

		#setting up the layout, putting the treeview in a scrollwindow, and the buttons in a row
		self.scrollable_treelist = Gtk.ScrolledWindow()
		self.scrollable_treelist.set_vexpand(True)
	

		self.labels = list()
		for lbls in ["Address","Information"]:
			label = Gtk.Label(lbls)
			self.labels.append(label)

		self.box = Gtk.Box(spacing=6, homogeneous=False)
		self.address = Gtk.Entry()
		self.address.set_text("own")
		self.box.pack_start(self.address, True, True, 0)
		self.button_request_transaction = Gtk.Button(label="Request")
		self.button_request_transaction.connect("clicked", self.on_click_find_transactions)
		self.box.pack_start(self.button_request_transaction, True, True, 0)

		self.transactions_generate = Gtk.Box(homogeneous=False,spacing = 6)

		self.transactions_generate.set_size_request(400,150)
		#!self.transactions_generate.set_min_height(100)
		self.transactions_label_generate = Gtk.Label("Transactions:")
		self.transactions_generate.pack_start(self.transactions_label_generate, False, False, 0)
		self.transactions_label_generate.set_size_request(100,-1)
		self.transaction_liststore = Gtk.ListStore(str,str)
		#self.transaction_liststore.append(["Save", Gtk.STOCK_CANCEL])	

		
		#creating the treeview, making it use the filter as a model, and adding the columns
		self.transaction_list_generate = Gtk.TreeView.new_with_model(self.transaction_liststore)
		#self.treeview.set_height_request(100)
		for i, column_title in enumerate(["Transactions"]):
		    renderer = Gtk.CellRendererText()
		    column = Gtk.TreeViewColumn(column_title, renderer, text=i)
		    column.set_min_width(300)
		    self.transaction_list_generate.append_column(column)
		
		renderer_pixbuf = Gtk.CellRendererPixbuf()
		column_pixbuf = Gtk.TreeViewColumn("Image", renderer_pixbuf, stock_id=1)
       		self.transaction_list_generate.append_column(column_pixbuf)
		
		self.transactions_generate.pack_start(self.transaction_list_generate, True, True, 0)
	
		self.values_generate = Gtk.Box(homogeneous=False,spacing = 6)	
		self.values_generate.set_size_request(400,200)
		self.values_label_generate = Gtk.Label("Output:   ")
		self.values_generate.pack_start(self.values_label_generate, False, False, 0)
		self.values_label_generate.set_size_request(100,-1)
		self.values_liststore = Gtk.ListStore(str, str, str, str)
		
		self.values_liststore.append([" ", " ", " ", Gtk.STOCK_ADD])	
		
		self.values_list_generate = Gtk.TreeView.new_with_model(self.values_liststore)
		self.values_list_generate.connect('row-activated',self.newReceiveForm)
		#self.treeview.set_height_request(100)
		for i, column_title in enumerate(["Values", "Addresses", "Proofs"]):
		    renderer = Gtk.CellRendererText()
		    column = Gtk.TreeViewColumn(column_title, renderer, text=i)
		    self.values_list_generate.append_column(column)

		renderer_pixbuf = Gtk.CellRendererPixbuf()
		column_pixbuf1 = Gtk.TreeViewColumn("Image", renderer_pixbuf, stock_id=3)
       		self.values_list_generate.append_column(column_pixbuf1)    
		self.values_generate.pack_start(self.values_list_generate, True, True, 0)
	
		
		self.run_generate = Gtk.Box(spacing = 6)
		self.run_entry_generate = Gtk.Entry()
		self.run_entry_generate.set_text("Information")
		self.run_generate.pack_start(self.run_entry_generate, True, True, 0)
		self.run_button_generate = Gtk.Button(label = "Run")
		self.run_button_generate.set_tooltip_text("Run")
		self.run_generate.pack_start(self.run_button_generate, True, True, 0)
	
		self.grid.attach(self.labels[0],0,0,1,1)
		self.grid.attach_next_to (self.labels[1],self.labels[0],Gtk.PositionType.RIGHT,1,1)	
		self.grid.attach(self.box, 0,1,1,2)
		self.grid.attach(self.scrollable_treelist, 0,3,1,5)
		self.grid.attach(self.transactions_generate, 1,3,1,1)
		self.grid.attach(self.values_generate, 1,4,1,1)
		self.grid.attach(self.run_generate, 1,5,1,1)
		#self.grid.attach_next_to(self.buttons[0], self.scrollable_treelist, Gtk.PositionType.BOTTOM, 1, 1)
		#for i, button in enumerate(self.buttons[1:]):
		 #   self.grid.attach_next_to(button, self.buttons[i], Gtk.PositionType.RIGHT, 1, 1)
		self.scrollable_treelist.add(self.treeview)

		self.page1 = Gtk.Box()
		self.page1.set_border_width(10)
		self.page1.add(self.grid)
		self.main_layer.append_page(self.page1, Gtk.Label('Generate Transaction'))
		self.page2 = Gtk.Box()
		self.page2.set_border_width(10)
		self.main_layer.append_page(self.page2, Gtk.Label('Parse Transaction'))
		self.page3 = Gtk.Box()
		self.page3.set_border_width(10)
		self.main_layer.append_page(self.page3, Gtk.Label('Verify Transaction'))


		#self.addressRequest(author)
		self.show_all()

	def language_filter_func(self, model, iter, data):
		"""Tests if the language in the row is the one in the filter"""
		if self.current_filter_language is None or self.current_filter_language == "None":
		    return True
		else:
		    return model[iter][2] == self.current_filter_language

	def addToTransactionList(self, element):
		self.software_liststore.append(list(element))

	def ConstructAddresses(self, transactionList):
		lst = list()
		parsedTransactions = transactionList["txs"] #list
		for i in range (len(parsedTransactions)):
			lst.append((i, transactionList["txs"][i]["hash"]))
		self.software_liststore.clear()
		for i in lst:
			self.addToTransactionList(i)

	def request(self, address):
		print(address)
		req = urllib2.Request(url=address)
		f = urllib2.urlopen(req)
		r = f.read()
		parsed = json.loads(r)
		return parsed

	def addressRequest(self, address):
		link = 'https://blockchain.info/ru/rawaddr/' + address
		parsed = request (link)
		lvtext = self.ConstructAddresses(parsed)
		return lvtext

	def on_click_find_transactions(self, widget):
		if (self.address.get_text() == 'own'):
			self.addressRequest(author)
		else:
			self.addressRequest(self.address.get_text())
		

	def on_selection_button_clicked(self, widget):
		"""Called on any of the button clicks"""
		#we set the current language filter to the button's label
		self.current_filter_language = widget.get_label()
		print("%s language selected!" % self.current_filter_language)
		#we update the filter, which updates in turn the view
		self.language_filter.refilter()


win = TreeViewFilterWindow()
win.connect("delete-event", Gtk.main_quit)
win.show_all()
Gtk.main()
