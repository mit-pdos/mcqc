#ifndef EXCEPTION_H
#define EXCEPTION_H
#include <stdexcept>
#include <string>

// IO Exceptions, files, network, sockets etc.
struct IOException: std::runtime_error {
	IOException(const std::string &msg): std::runtime_error(msg) {};
};

// Overflows
struct OverflowException: std::runtime_error {
	OverflowException(const std::string &msg): std::runtime_error(msg) {};
};
#endif
