import React from 'react'
import PropTypes from 'prop-types'

import Rounded from '../misc/rounded.jsx'
import ItemHeader from '../misc/itemheader.jsx'

const DatasetList = props =>
    (
        <div>
            {props.datasets.map(dataset => (
                <Rounded>
                    <ItemHeader
                        text={dataset.name}
                        close={() => props.deleteDataset(dataset.name)}
                    />
                </Rounded>))
            }
        </div>
    )

DatasetList.propTypes = {
    datasets: PropTypes.arrayOf(PropTypes.shape({
        name: PropTypes.string,
    })).isRequired,
}

export default DatasetList
