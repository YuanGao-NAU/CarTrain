from abc import abstractmethod, ABCMeta

class Model(metaclass=ABCMeta):
    
    def __init__(self):
        pass

    @abstractmethod
    def get_x(self):
        pass

    @abstractmethod
    def right_hand_equation(self, t, y):
        pass

    @abstractmethod
    def write_output(self, filename):
        pass

    @abstractmethod
    def ode_solver(self):
        pass