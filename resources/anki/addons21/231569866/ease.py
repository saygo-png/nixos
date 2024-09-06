from enum import Enum


class Ease(Enum):
    Again = 1
    Hard = 2
    Good = 3
    Easy = 4

    @staticmethod
    def from_num(ease: int, button_count: int) -> "Ease":
        if button_count == 2:
            if ease == 1:
                return Ease.Again
            else:
                return Ease.Good
        elif button_count == 3:
            if ease == 1:
                return Ease.Again
            elif ease == 2:
                return Ease.Good
            else:
                return Ease.Easy
        else:
            if ease == 1:
                return Ease.Again
            elif ease == 2:
                return Ease.Hard
            elif ease == 3:
                return Ease.Good
            else:
                return Ease.Easy
