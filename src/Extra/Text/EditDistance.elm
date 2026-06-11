module Extra.Text.EditDistance exposing (distance)

import Array exposing (Array)


distance : String -> String -> Int
distance left right =
    let
        leftChars =
            Array.fromList (String.toList left)

        rightChars =
            Array.fromList (String.toList right)

        leftLength =
            Array.length leftChars

        rightLength =
            Array.length rightChars

        sharedPrefix =
            matchingPrefixLength leftChars rightChars leftLength rightLength

        sharedSuffix =
            matchingSuffixLength leftChars rightChars leftLength rightLength sharedPrefix

        trimmedLeftLength =
            leftLength - sharedPrefix - sharedSuffix

        trimmedRightLength =
            rightLength - sharedPrefix - sharedSuffix
    in
    if trimmedLeftLength == 0 then
        trimmedRightLength

    else if trimmedRightLength == 0 then
        trimmedLeftLength

    else
        computeDistance
            (Array.slice sharedPrefix (leftLength - sharedSuffix) leftChars)
            (Array.slice sharedPrefix (rightLength - sharedSuffix) rightChars)
            trimmedLeftLength
            trimmedRightLength


matchingPrefixLength : Array Char -> Array Char -> Int -> Int -> Int
matchingPrefixLength leftChars rightChars leftLength rightLength =
    let
        limit =
            min leftLength rightLength

        loop index =
            if index >= limit then
                limit

            else if getChar index leftChars == getChar index rightChars then
                loop (index + 1)

            else
                index
    in
    loop 0


matchingSuffixLength : Array Char -> Array Char -> Int -> Int -> Int -> Int
matchingSuffixLength leftChars rightChars leftLength rightLength sharedPrefix =
    let
        limit =
            min (leftLength - sharedPrefix) (rightLength - sharedPrefix)

        loop offset =
            if offset >= limit then
                limit

            else if
                getChar (leftLength - offset - 1) leftChars
                    == getChar (rightLength - offset - 1) rightChars
            then
                loop (offset + 1)

            else
                offset
    in
    loop 0


computeDistance : Array Char -> Array Char -> Int -> Int -> Int
computeDistance leftChars rightChars leftLength rightLength =
    let
        initialRow =
            Array.initialize (rightLength + 1) identity
    in
    if leftLength == 1 then
        buildFirstRow leftChars rightChars 1 rightLength initialRow
            |> getInt rightLength

    else
        let
            firstRow =
                buildFirstRow leftChars rightChars 1 rightLength initialRow

            loopRows rowIndex previousPreviousRow previousRow =
                if rowIndex > leftLength then
                    getInt rightLength previousRow

                else
                    let
                        currentRow =
                            buildRowWithTransposition leftChars rightChars rowIndex rightLength previousPreviousRow previousRow
                    in
                    loopRows (rowIndex + 1) previousRow currentRow
        in
        loopRows 2 initialRow firstRow


buildFirstRow : Array Char -> Array Char -> Int -> Int -> Array Int -> Array Int
buildFirstRow leftChars rightChars rowIndex rightLength previousRow =
    let
        currentChar =
            getChar (rowIndex - 1) leftChars

        step columnIndex leftCost reversedRow =
            if columnIndex > rightLength then
                reversedRow
                    |> List.reverse
                    |> Array.fromList

            else
                let
                    rightChar =
                        getChar (columnIndex - 1) rightChars

                    substitutionCost =
                        if currentChar == rightChar then
                            0

                        else
                            1

                    deletion =
                        getInt columnIndex previousRow + 1

                    insertion =
                        leftCost + 1

                    substitution =
                        getInt (columnIndex - 1) previousRow + substitutionCost

                    cell =
                        min deletion (min insertion substitution)
                in
                step (columnIndex + 1) cell (cell :: reversedRow)
    in
    step 1 rowIndex [ rowIndex ]


buildRowWithTransposition : Array Char -> Array Char -> Int -> Int -> Array Int -> Array Int -> Array Int
buildRowWithTransposition leftChars rightChars rowIndex rightLength previousPreviousRow previousRow =
    let
        currentChar =
            getChar (rowIndex - 1) leftChars

        previousChar =
            getChar (rowIndex - 2) leftChars

        step columnIndex leftCost reversedRow =
            if columnIndex > rightLength then
                reversedRow
                    |> List.reverse
                    |> Array.fromList

            else
                let
                    rightChar =
                        getChar (columnIndex - 1) rightChars

                    substitutionCost =
                        if currentChar == rightChar then
                            0

                        else
                            1

                    deletion =
                        getInt columnIndex previousRow + 1

                    insertion =
                        leftCost + 1

                    substitution =
                        getInt (columnIndex - 1) previousRow + substitutionCost

                    baseCost =
                        min deletion (min insertion substitution)

                    cell =
                        if
                            (columnIndex > 1)
                                && (currentChar == getChar (columnIndex - 2) rightChars)
                                && (previousChar == rightChar)
                        then
                            min baseCost (getInt (columnIndex - 2) previousPreviousRow + 1)

                        else
                            baseCost
                in
                step (columnIndex + 1) cell (cell :: reversedRow)
    in
    step 1 rowIndex [ rowIndex ]


getChar : Int -> Array Char -> Char
getChar index chars =
    case Array.get index chars of
        Just value ->
            value

        Nothing ->
            '\u{0000}'


getInt : Int -> Array Int -> Int
getInt index values =
    case Array.get index values of
        Just value ->
            value

        Nothing ->
            0
