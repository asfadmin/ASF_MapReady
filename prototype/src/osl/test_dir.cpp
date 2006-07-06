#include "osl/dir.h"
#include "osl/dir.cpp"

class verbose_dir : public osl::directory_recursive {
public:
	int depth;
	verbose_dir() {depth=1;}
	void indent(void) {
		for (int i=0;i<depth;i++) printf("  ");
	}
	void hit_file(const char *dirName,const char *fileName) {
		indent(); printf("%s\n",fileName);
	}
	void hit_directory(const char *dirName,const char *subdirName)
	{
		indent(); printf("%s/ {\n",subdirName);
		depth++;
		osl::directory_recursive::hit_directory(dirName,subdirName);
		depth--;
		indent(); printf("}\n");
	}
};

int main(int argc,char *argv[]) {
	verbose_dir d;
	d.list(argc>1?argv[1]:".");
}
