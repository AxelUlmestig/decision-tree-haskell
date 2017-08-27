import React from 'react';

import SectionHeader from '../misc/sectionheader.js';
import UploadButton from './uploadbutton.js';
import DatasetList from './datasetlist.js';

export default props =>
    <div>
        <SectionHeader value='Datasets'></SectionHeader>
        <DatasetList
            datasets={props.datasets}
            deleteDataset={props.deleteDataset}>
        </DatasetList>
        <UploadButton uploadDataset={props.uploadDataset}></UploadButton>
    </div>
