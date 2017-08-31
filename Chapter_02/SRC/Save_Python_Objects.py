x = 10
import pickle
pickle.dump(x,open("tu.tu","wb"))
# Assume a new session
isitx = pickle.load(open("tu.tu","rb"))
x==isitx
