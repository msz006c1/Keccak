#include <iostream>
#include <string>
#include <cstring>
#include "keccak.hpp"

int main(int argc, char *argv[]) {
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <algorithm> <input>" << std::endl;
        std::cerr << "Algorithms: sha3-256, sha3-512, sha3-1024" << std::endl;
        return 1;
    }

    const std::string algo = argv[1];
    const std::string input = argv[2];

    keccak::hash_result result = [&]() -> keccak::hash_result {
        if (algo == "sha3-256") {
            return keccak::sha3_256(input);
        } else if (algo == "sha3-512") {
            return keccak::sha3_512(input);
        } else if (algo == "sha3-1024") {
            return keccak::sha3_1024(input);
        } else {
            std::cerr << "Unknown algorithm: " << algo << std::endl;
            std::exit(2);
        }
    }();

    std::cout << result.to_hex() << std::endl;

    return 0;
}
