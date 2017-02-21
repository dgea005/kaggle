import numpy as np
import lightgbm as lgb
import pandas as pd
from scipy import sparse
from sklearn.metrics import log_loss
from sklearn.model_selection import GridSearchCV,StratifiedShuffleSplit
from sklearn.feature_extraction import text

def load_data(file):
    """load json and set index / time"""
    df = pd.read_json(file)
    df = df.set_index('listing_id')
    df['created'] = pd.to_datetime(df['created'])
    return df

print('loading train data...')
train = load_data('data/train.json')
print('loading test data...')
test = load_data('data/test.json')


def set_int_for_category(x):
    if x == 'low':
        return 0
    elif x == 'medium':
        return 1
    elif x == 'high':
        return 2

print('calculating some features...')
#### counting features
train['n_features'] = train.features.apply(lambda x: len(x))
test['n_features'] = test.features.apply(lambda x: len(x))

train['description_length'] = train.description.apply(lambda x: len(x))
test['description_length'] = test.description.apply(lambda x: len(x))
train['description_n_words'] = train.description.apply(lambda x: len([x for x in x.split(' ') if x != '']))
test['description_n_words'] = test.description.apply(lambda x: len([x for x in x.split(' ') if x != '']))

### time features
train['day_of_week'] = train['created'].dt.dayofweek
train['hour_of_day'] = train['created'].dt.hour
train['minute_of_hour'] = train['created'].dt.minute
train['second_of_minute'] = train['created'].dt.second

test['day_of_week'] = test['created'].dt.dayofweek
test['hour_of_day'] = test['created'].dt.hour
test['minute_of_hour'] = test['created'].dt.minute
test['second_of_minute'] = test['created'].dt.second

## manager and building id features
train.loc[:,'no_building_id'] = 0
train.loc[train.building_id == '0', 'no_building_id'] = 1
test.loc[:,'no_building_id'] = 0
test.loc[test.building_id == '0', 'no_building_id'] = 1

## some managers have all low interest almost
listings_per_manager_by_interest_level = (train
.groupby(['manager_id', 'interest_level'])
.size()
.reset_index()
.pipe(pd.pivot_table, index='manager_id', columns='interest_level', values=0)
.fillna(0)
.astype(int)[['low', 'medium', 'high']]
)
interest_proportions_per_manager = listings_per_manager_by_interest_level.divide(listings_per_manager_by_interest_level.sum(1), axis=0)

low_interest_manager_ids = interest_proportions_per_manager.loc[((listings_per_manager_by_interest_level.sum(1) > 10)
                                     & (interest_proportions_per_manager.low > 0.95))].index.values

train.loc[:,'low_interest_manager_id'] = 0
train.loc[train.manager_id.isin(low_interest_manager_ids), 'low_interest_manager_id'] = 1
test.loc[:,'low_interest_manager_id'] = 0
test.loc[test.manager_id.isin(low_interest_manager_ids), 'low_interest_manager_id'] = 1


by_building_id = (train
.loc[train.building_id != '0']
.groupby(['building_id', 'interest_level'])
.size()
.reset_index()
.pipe(pd.pivot_table, columns='interest_level', index='building_id', values=0)
.fillna(0).astype(int)
[['low', 'medium', 'high']]
)

by_building_proportions = by_building_id.divide(by_building_id.sum(1), axis=0)
low_interest_buildings = (by_building_proportions.
                            loc[((by_building_id.sum(1) > 10) & 
                                 (by_building_proportions.low > 0.95))])

train.loc[:,'low_interest_building_id'] = 0
train.loc[train.building_id.isin(low_interest_buildings.index.values), 'low_interest_building_id'] = 1
test.loc[:,'low_interest_building_id'] = 0
test.loc[test.building_id.isin(low_interest_buildings.index.values), 'low_interest_building_id'] = 1

## photos
train['n_photos'] = train.photos.apply(lambda x: len(x))
test['n_photos'] = test.photos.apply(lambda x: len(x))

train.loc[:,'has_zero_photos'] = 0
train.loc[train.n_photos == 0, 'has_zero_photos'] = 1
test.loc[:,'has_zero_photos'] = 0
test.loc[test.n_photos == 0, 'has_zero_photos'] = 1

## low interest displuy addresses
## need to clean this first...
display_x_tab = (train
 .groupby(['display_address', 'interest_level'])
 .size().reset_index()
 .pipe(pd.pivot_table, columns='interest_level', index='display_address', values=0)
 .fillna(0).astype(int)
 [['low', 'medium', 'high']]
)
display_x_tab_proportions = display_x_tab.divide(display_x_tab.sum(1), 0)
low_interest_addresses = display_x_tab_proportions.loc[((display_x_tab.sum(1) > 20) & (display_x_tab_proportions.low > 0.9))]

train.loc[:, 'low_interest_display_address'] = 0
train.loc[train.display_address.isin(low_interest_addresses.index.values), 'low_interest_display_address'] = 1
test.loc[:, 'low_interest_display_address'] = 0
test.loc[test.display_address.isin(low_interest_addresses.index.values), 'low_interest_display_address'] = 1

train['log_description_length'] = np.log(train.description_length + 0.01)
train['log_description_length'] = train['log_description_length'].fillna(0)

test['log_description_length'] = np.log(test.description_length + 0.01)
test['log_description_length'] = test['log_description_length'].fillna(0)

## features
## trying tf-idf code as from kaggle kernals
train['features_unlist'] = train["features"].apply(lambda x: " ".join(["_".join(i.lower().split(" ")) for i in x]))
test['features_unlist'] = test["features"].apply(lambda x: " ".join(["_".join(i.lower().split(" ")) for i in x]))

tfidf = text.CountVectorizer(stop_words='english', max_features=100)

train_ft_tfidf_transformed = tfidf.fit_transform(train["features_unlist"])
test_ft_tfidf_transformed = tfidf.transform(test["features_unlist"])


n_listings_per_manager = (train
.reset_index()
.groupby(['manager_id'], as_index=False)
.agg({'listing_id': lambda x: len(x.unique())})
.rename(columns={'listing_id': 'n_listings_of_manager'})
)


train = train.merge(n_listings_per_manager, on='manager_id')
test = test.merge(n_listings_per_manager, on='manager_id', how='left')
test.n_listings_of_manager = test.n_listings_of_manager.fillna(0)

print(train.reset_index().columns)
common_managers = (train.reset_index()
    .groupby('manager_id')
    .agg({'listing_id': lambda x: len(x)})
    .pipe(lambda df: df[df.listing_id > 50])
)

common_managers = common_managers.index.values
print(len(common_managers))

common_managers_train_pivot = (train.loc[train.manager_id.isin(common_managers)][['manager_id']]
.reset_index().assign(manager_val=1)
.pipe(pd.pivot_table, columns='manager_id', index='listing_id', values='manager_val')
.fillna(0).astype(int)
)

common_managers_test_pivot = (test.loc[test.manager_id.isin(common_managers)][['manager_id']]
.reset_index().assign(manager_val=1)
.pipe(pd.pivot_table, columns='manager_id', index='listing_id', values='manager_val')
.fillna(0).astype(int)
)

train = train.merge(common_managers_train_pivot, left_index=True, right_index=True, how='left')
# fillna
train.loc[:, common_managers] = train.loc[:, common_managers].fillna(0)
test = test.merge(common_managers_test_pivot, left_index=True, right_index=True, how='left')
# fillna
test.loc[:, common_managers] = test.loc[:, common_managers].fillna(0)



print('setting up features and test / train split')

features = ['bathrooms', 
            'bedrooms', 
            'latitude', 
            'longitude', 
            'price',
            'n_features',
            'no_building_id',
            'description_length',
            'log_description_length',
            'description_n_words',
            'day_of_week',
            'hour_of_day',
            'minute_of_hour',
            'second_of_minute',
            'low_interest_manager_id',
            'low_interest_building_id',
            'low_interest_display_address',
            'n_listings_of_manager']


X = sparse.hstack([train[features], train_ft_tfidf_transformed]).tocsr()
train['interest_level'] = train.interest_level.apply(lambda x: set_int_for_category(x))
y = train['interest_level']

sss = StratifiedShuffleSplit(n_splits = 2, test_size=0.35, random_state=0)
sss.get_n_splits(X=X, y=y)

for train_index, test_index in sss.split(X, y):
    print("TRAIN:", train_index, "TEST:", test_index)
    X_train, X_validate = X[train_index], X[test_index]
    y_train, y_validate = y.iloc[train_index], y.iloc[test_index]

# create dataset for lightgbm
lgb_train = lgb.Dataset(X_train, label=y_train)
lgb_eval = lgb.Dataset(X_validate, label=y_validate, reference=lgb_train)

print('data set has been setup')

# specify your configurations as a dict
params = {
    'task': 'train',
    'boosting_type': 'gbdt',
    'objective': 'multiclass',
    'metric': 'multi_logloss',
    'num_leaves': 100,
    'bagging_freq': 5,
    'verbose': 0,
    'num_class': 3,
    'max_bin': 5
    #'min_data_in_leaf': 5
    # 'feature_fraction': 0.3,
    # 'feature_fraction_seed': 42,
    # 'bagging_fraction': 0.8,
    # 'bagging_seed': 42
}

print('Start training...')
# train
gbm = lgb.train(params,
                lgb_train,
                num_boost_round=200,
                valid_sets=lgb_eval,
                learning_rates=lambda iter: 0.05 * (0.99 ** iter),
                early_stopping_rounds=5)

print('Save model...')
# save model to file
gbm.save_model('model.txt')

y_pred = gbm.predict(X_validate, num_iteration=gbm.best_iteration)

print(y_pred[0:5])

eval_error = log_loss(y_validate, y_pred) 
print(f'logloss of eval: {eval_error}')

# print('Calculate feature importances...')
# # feature importances
# print('Feature importances:', list(gbm.feature_importance()))

X_test = sparse.hstack([test[features], test_ft_tfidf_transformed]).tocsr()

y_test_predictions = gbm.predict(X_test,  num_iteration=gbm.best_iteration)

## create submission
test_predictions = pd.DataFrame({'listing_id': test.index.values})
test_predictions['low'] = y_test_predictions[:,0]
test_predictions['medium'] = y_test_predictions[:,1]
test_predictions['high'] = y_test_predictions[:,2]
test_predictions.to_csv('test_submission_msboost.csv', index=False)

