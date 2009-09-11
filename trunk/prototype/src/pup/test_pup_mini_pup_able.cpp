/*
 Mini STL map class test.
*/
#include <stdio.h>
#include <stdlib.h>
#include <map>

extern "C" void CmiAbort(const char *why) {
	printf("Fatal: %s\n",why);
	exit(1);
}

namespace PUP {

	//A globally-unique, persistent identifier for an allocatable object
	class PUP_ID {
	public:
		enum {len=8};
		unsigned char hash[len];
		PUP_ID() {}
		explicit PUP_ID(int val) {for (int i=0;i<len;i++) hash[i]=(unsigned char)val;}
		explicit PUP_ID(const char *name) {setName(name);}
		void setName(const char *name);//Write name into hash
		bool operator==(const PUP_ID &other) const {
			for (int i=0;i<len;i++)
				if (hash[i]!=other.hash[i])
					return false;
			return true;
		}
		bool operator<(const PUP_ID &other) const {
			for (int i=0;i<len;i++)
				if (hash[i]<other.hash[i])
					return true; /* really less */
				else if (hash[i]>other.hash[i])
					return false; /* really greater */
			return false; /* equal */
		}
		
	};

void PUP_ID::setName(const char *name) { 
#if 1
	int i,o,n=strlen(name);
	int t[len]={0};
	for (o=0;o<n;o++)
		for (i=0;i<len;i++) {
			unsigned char c=name[o];
			int shift1=(((o+2)*(i+1)*5+4)%13);
			int shift2=(((o+2)*(i+1)*3+2)%11)+13;
			t[i]+=(c<<shift1)+(c<<shift2);
		}
	for (i=0;i<len;i++) 
		hash[i]=(unsigned char)(t[i]%20117 + t[i]%1217 + t[i]%157);
#else
	for (int i=0;i<len;i++)
		hash[i]=name[0];
#endif
}

};

class PUP_regEntry {
public:
	PUP::PUP_ID id;
	PUP_regEntry(PUP::PUP_ID id_) :id(id_) {}
};

typedef std::map<PUP::PUP_ID,PUP_regEntry *> PUP_registry;

static PUP_registry *PUP_getRegistry(void) {
	static PUP_registry *reg=NULL;
	if (reg==NULL)
		reg=new PUP_registry();
	return reg;
}

static void print(const PUP::PUP_ID &id) {
	printf("ID: ");
	for (int i=0;i<PUP::PUP_ID::len;i++)
		printf("%02x",id.hash[i]);
}
static void print(const PUP_regEntry *r) {
	printf("  regEntry ");
	print(r->id);
	printf("\n");
}
static void print(PUP_registry &r) {
	printf("PUP::able registry contents: %d entries\n",r.size());
	for (PUP_registry::iterator it=r.begin();it!=r.end();++it)
		print((*it).second);
}

PUP::PUP_ID add(const char *name) {
	PUP::PUP_ID id(name);
	if (id<id) CmiAbort("ERROR! self-compare on id fails!\n");
	PUP_registry *r=PUP_getRegistry();
	PUP_regEntry *e=new PUP_regEntry(id);
	PUP_registry::iterator it=r->find(id);
	if (it!=r->end()) {
		printf("ERROR! Duplicate PUP::able names/ids registered!\n");
		printf("Old: "); print((*it).second);
		printf("New: "); print(e);
		CmiAbort("ERROR! Duplicate PUP::able names/ids registered!\n");
	}
	r->insert(std::make_pair(id,e));
	//r[0][id]=e;
	//(*r)[id]=e;
	if (1) { 
		printf("Debugging PUP::able::register_constructor...\n adding");
		print(id);
		print(e);
		print(*r);
	}
	it=r->find(id);
	if (it==r->end()) CmiAbort("ERROR! Added ID, but still not there!\n");
	if ((*it).second!=e) CmiAbort("ERROR! Added ID, but something else there!\n");
	printf("\n");
	return id;
}

int main() {
	PUP::PUP_ID a("1"), b("2");
	printf("a=, b="); print(a); print(b);
	printf(" a<b = %d,  b<a = %d\n", a<b, b<a);

	add("1");
	add("2");
	add("3");
	add("4");
}

