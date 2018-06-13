#include <iostream>
#include <clang-c/Index.h>
#include <sstream>      // std::stringstream
#include <vector>
#include <cstring>
#include <sys/stat.h>
#include <libgen.h>

using namespace std;

// global file pointer, workaround for non-binding lambda in visitChildren
char *currentFilename = NULL;
// Global state, capturing changes the visitor signature
stringstream os;

inline bool file_exists(const char *name) {
  struct stat buffer;
  return (stat (name, &buffer) == 0);
}


static inline string Convert(const CXString& s)
{
    string result = clang_getCString(s);
    clang_disposeString(s);
    return result;
}

vector<string> split(string str, string token){
    vector<string>result;
    while(str.size()){
        int index = str.find(token);
        if(index!=string::npos){
            result.push_back(str.substr(0,index));
            str = str.substr(index+token.size());
            if(str.size()==0)result.push_back(str);
        } else{
            result.push_back(str);
            str = "";
        }
    }
    return result;
}

string argsToJSON(string s) {
    unsigned int diff = s.find(')') - s.find('(') - 1;
    s = s.substr(s.find('(')+1, diff);
    stringstream sss;
    sss << "\"";
    vector<string> v = split(s, ", ");
    for(int i = 0; i < v.size(); ++i){
        if(i)
            sss << "\", \"";
        sss << v[i];
    }
    sss << "\"";
    return sss.str();
}

int funcToJSON(CXCursor C) {
	// Catch only function declarations
	if (!clang_isDeclaration(C.kind))
    	return -1;

	// cout << clang_getCString(clang_Cursor_getArgument(C, 0)) << endl;
    auto type = clang_getCursorType(C);
    auto function_name = Convert(clang_getCursorSpelling(C));
    auto return_type   = Convert(clang_getTypeSpelling(clang_getResultType(type)));
	auto signature = Convert(clang_getCursorDisplayName(C));

	// begin function JSON record
	os << "{ \"name\": \"" << function_name << "\", \"typ\" : \"" << return_type << "\", \"args\" : [ ";
	os << argsToJSON(signature);
	os << " ]}, " << endl;
	return 0;
}

// Run for one Hpp file
extern "C" char* clangToJSON(const char *fn) {
  // Overwrite the global file pointer
  currentFilename = basename(const_cast<char*>(fn));
  os.str(std::string());

  // Check if file exists to give a better error
  if (!file_exists(fn)) {
	cerr << "Clangjson error: File not found: " << fn << endl;
	exit(-2);
  }

  // Set clang arguments
  CXIndex index = clang_createIndex(0, 0);
  constexpr const char* defaultArguments[] = {
    "-std=c++0x",
    "-fsyntax-only",
    "-Wno-switch-bool"
  };

  // translate
  CXTranslationUnit unit = clang_parseTranslationUnit( index,
                                              fn,
                                              defaultArguments,
                                              std::extent<decltype(defaultArguments)>::value,
                                              0,
                                              0,
                                              CXTranslationUnit_None );
  if (unit == nullptr)
  {
    cerr << "Clangjson error: Unable to parse translation unit: " << fn << endl;
    exit(-1);
  }

  CXCursor cursor = clang_getTranslationUnitCursor(unit);
  clang_visitChildren(
    cursor,
    [](CXCursor c, CXCursor parent, CXClientData client_data)
    {
      // If the cursor is in the main file (no system libs)
      if(clang_Location_isFromMainFile(clang_getCursorLocation(c)) == 0)
        return CXChildVisit_Continue;

	  // And it's a namespace with the same name as the file
      auto ns = Convert(clang_getCursorSpelling(c));
      if (clang_getCursorKind(c) == CXCursor_Namespace) {
        char *filenm = new char[strlen(ns.c_str()) + 1];
        strncpy(filenm, currentFilename, strlen(ns.c_str()));
		if (strncmp(ns.c_str(), filenm, strlen(filenm))) {
          free(filenm);
		  return CXChildVisit_Continue;
        }
        else {
          free(filenm);
          os << "{ \"namespace\" : \"" << ns << "\", \"functions\" : [  " << endl;
		}
      }

	  // Write it to file with all its functions
	  if (clang_getCursorKind(c) == CXCursor_FunctionDecl) {
        funcToJSON(c);
      }
      // Match template functions
      if (clang_getCursorKind(c) == CXCursor_FunctionTemplate) {
		funcToJSON(c);
      }

      return CXChildVisit_Recurse;
    },
    nullptr);

  // Erase last comma
  os.seekp(-3, os.cur);
  os << "]}";

  clang_disposeTranslationUnit(unit);
  clang_disposeIndex(index);
  // Heap allocate
  char* cstr = new char [os.str().length()+1];
  std::strcpy(cstr, os.str().c_str());
  return cstr;
}

