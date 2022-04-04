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

    def set_location(self, location: str):
        '''adds a location to the location list'''
        if not location in self.library and location != "":
            self.library[location] = {}
        self.location.append(location)

    def pop_location(self):
        '''removes the last location from the location list thus going back one location'''
        self.location.pop()

    def get_location(self):
        '''returns the current location'''
        return self.location[-1]

    def set_value(self, key: str, value: any):
        '''sets a value in library with a location and a key'''
        if self.location[-1] == "":
            self.library[key] = value
        else:
            self.library[self.location[-1]][key] = value

    def get_value(self, key: str):
        '''gets a value from library with a location and a key'''
        if self.location[-1] == "":
            return self.library[key]
        # if key cannot be found on location try to find it on the global library
        if not key in self.library[self.location[-1]]:
            return self.library[key]
        return self.library[self.location[-1]][key]

    def key_exists(self, key: str):
        '''checks if a key exists in library with a location and a key'''
        if self.location[-1] == "":
            return key in self.library
        return key in self.library[self.location[-1]]


if __name__ == '__main__':
    z = Library()
    z.set_value("test", {})
    z.set_location("test")
    z.set_value("test", 2)
    z.set_location("")
    print(z["test"]["test"])