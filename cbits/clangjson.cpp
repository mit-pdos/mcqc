#include <iostream>
#include <clang-c/Index.h>
#include <sstream>      // std::stringstream
#include <cstring>
#include <sys/stat.h>
#include <libgen.h>

using namespace std;

// Global state, capturing changes the visitor signature
stringstream os;

static inline string Convert(const CXString& s)
{
    string result = clang_getCString(s);
    clang_disposeString(s);
    return result;
}

void print_func_bundle(CXCursor cursor, stringstream& os) {
    auto type = clang_getCursorType(cursor);
    auto function_name = Convert(clang_getCursorSpelling(cursor));
    auto return_type   = Convert(clang_getTypeSpelling(clang_getResultType(type)));

    int num_args = clang_Cursor_getNumArguments(cursor);

	// begin function JSON record
	os << "{ \"name\": \"" << function_name << "\", \"typ\" : \"" << return_type << "\", \"args\" : [ ";
    for (int i = 0; i < num_args; ++i)
    {
        auto arg_cursor = clang_Cursor_getArgument(cursor, i);
        auto arg_name = Convert(clang_getCursorSpelling(arg_cursor));
        if (arg_name.empty()) {
            arg_name = "_";
        }
        auto arg_data_type = Convert(clang_getTypeSpelling(clang_getArgType(type, i)));
		// append function arguments
		os << "\"" << arg_data_type << "\"";
		if (i < num_args - 1) {
			os << ", ";
		}
    }
	os << " ]}, " << endl;
}

inline bool file_exists(const char *name) {
  struct stat buffer;
  return (stat (name, &buffer) == 0);
}

// global file pointer, workaround for non-binding lambda in visitChildren
char *currentFilename = NULL;

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
        print_func_bundle(c, os);
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

