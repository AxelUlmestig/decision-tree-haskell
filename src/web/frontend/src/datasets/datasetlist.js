import React from 'react';

import Rounded from '../misc/rounded.js';
import ItemHeader from '../misc/itemheader.js';

const DatasetList = props =>
    <div>
        {props.datasets.map(dataset =>
            <Rounded>
                <ItemHeader
                    text={dataset.name}
                    close={() => props.deleteDataset(dataset.name)}>
                </ItemHeader>
            </Rounded>
        )}
    </div>

export default DatasetList
