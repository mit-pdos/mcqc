#include "clangjson.cpp"
#include <string>
#include <iostream>
#include <experimental/filesystem>

namespace fs = std::experimental::filesystem;
int main()
{
    for (auto & p : fs::directory_iterator("../include")) {
        auto path = p.path();
		if (path.extension() == ".hpp") {
			std::cout << "========== File: " << p << "======+===" << std::endl;
			std::cout << clangToJSON(path.c_str()) << std::endl;
			std::cout << std::endl;
		}
	}
}

