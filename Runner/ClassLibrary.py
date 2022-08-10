from typing import List

class Library():
    def __init__(self) -> None:
        self.library: dict = {}
        self.location: List[str] = [""]
    
    def __str__(self) -> str:
        return self.__repr__()
    
    def __repr__(self) -> str:
        return f"Library(library: {self.library})"

    def __getitem__(self, key):
        return self.get_value(key)

    def __setitem__(self, key, value):
        self.set_value(key, value)

    def __contains__(self, key):
        return self.key_exists(key)

    # set_location :: str -> None
    def set_location(self, location: str):
        '''adds a location to the location list'''
        if not location in self.library and location != "":
            self.library[location] = {}
        self.location.append(location)

    # pop :: str -> any
    def pop(self, key: str) -> any:
        '''removes a key from library with a location and a key and returns the value'''
        if self.location[-1] == "":
            return self.library.pop(key)
        return self.library[self.location[-1]].pop(key)

    # pop_location :: None -> None
    def pop_location(self):
        '''removes the last location from the location list thus going back one location'''
        self.location.pop()

    # get_location :: None -> None
    def get_location(self):
        '''returns the current location'''
        return self.location[-1]

    # set_value :: str -> any -> None
    def set_value(self, key: str, value: any):
        '''sets a value in library with a key and value'''
        if self.location[-1] == "":
            self.library[key] = value
        else:
            self.library[self.location[-1]][key] = value

    # get_value :: str -> any
    def get_value(self, key: str) -> any:
        '''gets a value from library with a key and value'''
        # If the key exists in the current location
        if self.location[-1] != "": 
            return self.library[self.location[-1]][key]
        # If the key exists in the global library
        elif self.key_exists(key):
            return self.library[key]
        else:
            raise KeyError(f"Key {key} does not exist in library")

    # key_exists :: str -> bool
    def key_exists(self, key: str) -> bool:
        '''checks if a key exists in library with a location and a key or in the global library'''
        if self.location[-1] != "":
            return key in self.library[self.location[-1]] or key in self.library
        return key in self.library


if __name__ == '__main__':
    z = Library()
    z.set_value("test", {})
    z.set_location("test")
    z.set_value("test", 2)
    z.set_location("")
    print(z["test"]["test"])